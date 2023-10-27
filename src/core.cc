#include <core.hh>
#include <fmt/color.h>
#include <unordered_map>

auto utils::Drain(FILE* f) -> std::string {
    std::string contents;
    static constexpr usz bufsize = 4'096;
    for (;;) {
        contents.resize(contents.size() + bufsize);
        auto read = std::fread(contents.data() + contents.size() - bufsize, 1, bufsize, f);
        if (read < bufsize) contents.resize(contents.size() - (bufsize - read));
        if (std::ferror(f)) Diag::Fatal("Error reading file: {}", std::strerror(errno));
        if (std::feof(f)) break;
    }
    return contents;
}

void utils::ReplaceAll(
    std::string& str,
    std::string_view from,
    std::string_view to
) {
    if (from.empty()) return;
    for (usz i = 0; i = str.find(from, i), i != std::string::npos; i += to.length())
        str.replace(i, from.length(), to);
}

auto utils::NumberWidth(usz number, usz base) -> usz {
    return number == 0 ? 1 : usz(std::log(number) / std::log(base) + 1);
}

namespace {
constexpr void TrimFront(std::string_view& sv) {
    while (not sv.empty() and utils::IsWhitespace(sv.front())) sv.remove_prefix(1);
}
}

/// ===========================================================================
///  Location
/// ===========================================================================
bool Location::seekable() const {
    return is_valid() and pos + len <= file->contents.size();
}

/// Seek to a source location. The location must be valid.
auto Location::seek() const -> LocInfo {
    LocInfo info{};

    /// Seek back to the start of the line.
    const char* const data = file->contents.data();
    info.line_start = data + pos;
    while (info.line_start > data and *info.line_start != '\n') info.line_start--;
    if (*info.line_start == '\n') info.line_start++;

    /// Seek forward to the end of the line.
    const char* const end = data + file->contents.size();
    info.line_end = data + pos;
    while (info.line_end < end and *info.line_end != '\n') info.line_end++;

    /// Determine the line and column number.
    info.line = 1;
    for (const char* d = data; d < data + pos; d++) {
        if (*d == '\n') {
            info.line++;
            info.col = 0;
        } else {
            info.col++;
        }
    }

    /// Done!
    return info;
}

auto Location::seek_line_column() const -> LocInfoShort {
    LocInfoShort info{};

    /// Seek back to the start of the line.
    const char* const data = file->contents.data();

    /// Determine the line and column number.
    info.line = 1;
    for (const char* d = data; d < data + pos; d++) {
        if (*d == '\n') {
            info.line++;
            info.col = 0;
        } else {
            info.col++;
        }
    }

    /// Done!
    return info;
}

/// ===========================================================================
///  Diagnostics
/// ===========================================================================
namespace {
/// Get the colour of a diagnostic.
constexpr auto Colour(Diag::Kind kind) {
    using Kind = Diag::Kind;
    switch (kind) {
        case Kind::ICError: return fmt::fg(fmt::terminal_color::magenta) | fmt::emphasis::bold;
        case Kind::Warning: return fmt::fg(fmt::terminal_color::yellow) | fmt::emphasis::bold;
        case Kind::Note: return fmt::fg(fmt::terminal_color::green) | fmt::emphasis::bold;

        case Kind::FError:
        case Kind::Error:
            return fmt::fg(fmt::terminal_color::red) | fmt::emphasis::bold;

        default:
            return fmt::text_style{};
    }
}

/// Get the name of a diagnostic.
constexpr std::string_view Name(Diag::Kind kind) {
    using Kind = Diag::Kind;
    switch (kind) {
        case Kind::ICError: return "Internal Compiler Error";
        case Kind::FError: return "Fatal Error";
        case Kind::Error: return "Error";
        case Kind::Warning: return "Warning";
        case Kind::Note: return "Note";
        default: return "Diagnostic";
    }
}

auto NormaliseFilename(std::string_view filename) -> std::string_view {
    if (auto pos = filename.find(FCHK_PROJECT_DIR_NAME); pos != std::string_view::npos) {
        static constexpr std::string_view name{FCHK_PROJECT_DIR_NAME};
        filename.remove_prefix(pos + name.size() + 1);
    }
    return filename;
}

} // namespace

/// Abort due to assertion failure.
void detail::AssertFail(
    AssertKind k,
    std::string_view condition,
    std::string_view file,
    int line,
    std::string&& message
) {
    using fmt::fg;
    using enum fmt::emphasis;
    using enum fmt::terminal_color;

    /// Print filename and ICE title.
    fmt::print(stderr, bold, "{}:{}:", file, line);
    fmt::print(stderr, Colour(Diag::Kind::ICError), " {}: ", Name(Diag::Kind::ICError));

    /// Print the condition, if any.
    switch (k) {
        case AssertKind::AK_Assert:
            fmt::print(stderr, "Assertion failed: '{}'", condition);
            break;

        case AssertKind::AK_Todo:
            fmt::print(stderr, "TODO");
            break;

        case AssertKind::AK_Unreachable:
            fmt::print(stderr, "Unreachable code reached");
            break;
    }

    /// Print the message.
    if (not message.empty()) fmt::print(stderr, ": {}", message);
    fmt::print("\n");

    /// Print the backtrace and exit.
    std::exit(Diag::ICEExitCode);
}

void Diag::HandleFatalErrors() {
    /// Abort on ICE.
    if (kind == Kind::ICError)
        std::exit(ICEExitCode);

    /// Exit on a fatal error.
    if (kind == Kind::FError)
        std::exit(FatalExitCode); /// Separate line so we can put a breakpoint here.
}

/// Print a diagnostic with no (valid) location info.
void Diag::PrintDiagWithoutLocation() {
    /// Print the message.
    fmt::print(stderr, Colour(kind), "{}: ", Name(kind));
    fmt::print(stderr, "{}\n", msg);
    HandleFatalErrors();
}

Diag::~Diag() { print(); }

void Diag::print() {
    using fmt::fg;
    using enum fmt::emphasis;
    using enum fmt::terminal_color;

    /// If this diagnostic is suppressed, do nothing.
    if (kind == Kind::None) return;

    /// Don’t print the same diagnostic twice.
    defer { kind = Kind::None; };

    /// If the diagnostic is an error, set the error flag.
    if (kind == Kind::Error and ctx)
        ctx->has_error = true; /// Separate line so we can put a breakpoint here.

    /// If there is no context, then there is also no location info.
    if (not ctx) {
        PrintDiagWithoutLocation();
        return;
    }

    /// If the location is invalid, either because the specified file does not
    /// exists, its position is out of bounds or 0, or its length is 0, then we
    /// skip printing the location.
    if (not where.seekable()) {
        /// Even if the location is invalid, print the file name if we can.
        if (where.file) fmt::print(stderr, bold, "{}: ", where.file->path.string());

        /// Print the message.
        PrintDiagWithoutLocation();
        return;
    }

    /// If the location is valid, get the line, line number, and column number.
    const auto [line, col, line_start, line_end] = where.seek();

    /// Split the line into everything before the range, the range itself,
    /// and everything after.
    std::string before(line_start, col);
    std::string range(line_start + col, std::min<u64>(where.len, u64(line_end - (line_start + col))));
    auto after = line_start + col + where.len > line_end
                   ? std::string{}
                   : std::string(line_start + col + where.len, line_end);

    /// Replace tabs with spaces. We need to do this *after* splitting
    /// because this invalidates the offsets.
    utils::ReplaceAll(before, "\t", "    ");
    utils::ReplaceAll(range, "\t", "    ");
    utils::ReplaceAll(after, "\t", "    ");

    /// Print the file name, line number, and column number.
    fmt::print(stderr, bold, "{}:{}:{}: ", where.file->path.string(), line, col);

    /// Print the diagnostic name and message.
    fmt::print(stderr, Colour(kind), "{}: ", Name(kind));
    fmt::print(stderr, "{}\n", msg);

    /// Print the line, if requested.
    if (print_line) {
        /// Print the line up to the start of the location, the range in the right
        /// colour, and the rest of the line.
        fmt::print(stderr, " {} │ {}", line, before);
        fmt::print(stderr, Colour(kind), "{}", range);
        fmt::print(stderr, "{}\n", after);

        /// Determine the number of digits in the line number.
        const auto digits = utils::NumberWidth(line);

        /// Determine the column width of the text.
        static const auto ColumnWidth = [](std::string_view text) {
            usz wd = 0;
            for (auto c : text) {
                if (std::iscntrl(c)) continue;
                else if (c == '\t') wd += 4;
                else wd++;
            }
            return wd;
        };

        /// Underline the range. For that, we first pad the line based on the number
        /// of digits in the line number and append more spaces to line us up with
        /// the range.
        for (usz i = 0, end = digits + ColumnWidth(before) + sizeof("  | ") - 1; i < end; i++)
            fmt::print(stderr, " ");

        /// Finally, underline the range.
        for (usz i = 0, end = ColumnWidth(range); i < end; i++)
            fmt::print(stderr, Colour(kind), "~");
        fmt::print(stderr, "\n");
    }

    /// Handle fatal errors.
    HandleFatalErrors();
}

/// ===========================================================================
///  Stream
/// ===========================================================================
auto Stream::operator[](usz start, usz end) const -> SV {
    start = std::min(start, text.size() - 1);
    end = std::min(end, text.size());
    return text.substr(start, end - start);
}

auto Stream::read_to(SV delim, bool discard) -> SV {
    auto pos = text.find(delim);
    if (pos == SV::npos) {
        auto ret = text;
        if (discard) text = "";
        return ret;
    }

    return yield_until(pos, discard);
}

auto Stream::read_to_any(SV delims, bool discard) -> SV {
    auto pos = text.find_first_of(delims);
    if (pos == SV::npos) {
        auto ret = text;
        if (discard) text = "";
        return ret;
    }

    return yield_until(pos, discard);
}

auto Stream::read_to_or_empty(SV delim, bool discard) -> SV {
    auto pos = text.find(delim);
    if (pos == SV::npos) {
        if (discard) text = "";
        return "";
    }

    return yield_until(pos, discard);
}

auto Stream::read_to_ws(bool discard) -> SV {
    return read_to_any(Whitespace, discard);
}

auto Stream::skip_to(SV delim) -> Stream& {
    auto pos = text.find(delim);
    if (pos == SV::npos) text = "";
    else text.remove_prefix(pos);
    return *this;
}

auto Stream::skip_to_any(SV delims) -> Stream& {
    auto pos = text.find_first_of(delims);
    if (pos == SV::npos) text = "";
    else text.remove_prefix(pos);
    return *this;
}

auto Stream::skip_to_ws() -> Stream& {
    return skip_to_any(Whitespace);
}

auto Stream::skip_ws() -> Stream& {
    auto pos = text.find_first_not_of(Whitespace);
    if (pos == SV::npos) text = "";
    else text.remove_prefix(pos);
    return *this;
}

auto Stream::yield_until(usz pos, bool remove) -> SV {
    auto ret = text.substr(0, pos);
    if (remove) text.remove_prefix(pos);
    return ret;
}

/// ===========================================================================
///  Implementation
/// ===========================================================================
auto Context::LocationIn(std::string_view sv, File& file) const -> Location {
    auto start = sv.data() - file.contents.data();
    return Location{u32(start), u16(sv.size()), &file};
}

int Context::Run() {
    static std::unordered_map<std::string_view, Directive> name_directive_map{
        {DirectiveNames[+Directive::CheckAny], Directive::CheckAny},
        {DirectiveNames[+Directive::CheckNext], Directive::CheckNext},
        {DirectiveNames[+Directive::Prefix], Directive::Prefix},
        {DirectiveNames[+Directive::Run], Directive::Run},
    };

    /// If we don’t know what the prefix is, look for a
    /// prefix directive.
    Stream chfile{check_file.contents};
    const bool have_command_line_prefix = not prefix.empty();
    if (not have_command_line_prefix) {
        prefix = Trim(
            auto{chfile}
                .skip_to(DirectiveNames[+Directive::Prefix])
                .skip_to_ws()
                .skip_ws()
                .read_to("\n")
        );

        if (prefix.empty()) Diag::Fatal(
            "No prefix provided and no {} directive found in check file",
            DirectiveNames[+Directive::Prefix]
        );
    }

    /// Collect check directives.
    for (;;) {
        /// Read directive.
        auto dir = Trim(
            chfile
                .skip_to(prefix)
                .skip_to_ws()
                .skip_ws()
                .read_to_ws()
        );
        if (dir.empty()) break;

        /// Check if this really is a directive.
        auto it = name_directive_map.find(dir);
        if (it == name_directive_map.end()) continue;

        /// Read the directive’s argument.
        auto value = Trim(
            chfile
                .skip_to_ws()
                .skip_ws()
                .read_to("\n")
        );

        /// Handle spurious prefix directives
        if (it->second == Directive::Prefix) {
            /// A prefix provided on the command-line overrides the one
            /// in the file, if it is different.
            if (have_command_line_prefix) {
                if (prefix != value) {
                    Diag::Warning(
                        this,
                        LocationIn(dir, check_file),
                        "Prefix directive conflicts with prefix '{}' provided on the "
                        "command-line. Using the latter",
                        prefix
                    );
                }
            }

            /// Do not add a check for this.
            continue;
        }

        /// Run directive.
        if (it->second == Directive::Run) {
            run_directives.push_back(value);
            continue;
        }

        checks.emplace_back(it->second, value);
    }

    if (run_directives.empty()) Diag::Fatal(
        "No {} directives found in check file!",
        DirectiveNames[+Directive::Run]
    );

    for (auto rd : run_directives) RunTest(rd);
    return has_error ? 1 : 0;
}

bool Context::MatchLine(std::string_view line, std::string_view check_string) {
    Stream in{Trim(line)};
    Stream chk{Trim(check_string)};

    while (not in.empty() and not chk.empty()) {
        /// Matching ignores whitespace.
        if (chk.at_any(Stream::Whitespace)) {
            chk.skip_ws();
            in.skip_ws();
            continue;
        }

        /// Compare and discard the next word.
        auto chk_word = chk.read_to_ws(true);
        auto in_word = in.read_to_ws(true);
        if (chk_word != in_word) return false;
    }

    /// Trailing data in a line is allowed.
    return chk.empty();
}

void Context::RunTest(std::string_view test) {
    static const auto MakeStringView = [](auto&& r) {
        return std::string_view{&*r.begin(), usz(rgs::distance(r))};
    };

    /// Substitute occurrences of `%s` with the file name.
    auto cmd = std::string{test};
    utils::ReplaceAll(cmd, "%s", check_file.path.string());

    /// Run the command and get its output.
    auto pipe = ::popen(cmd.c_str(), "r");
    if (not pipe) Diag::Fatal("Failed to run command '{}': {}", cmd, std::strerror(errno));
    File input_file{utils::Drain(pipe), "<input>"};
    ::pclose(pipe);

    /// Matchine state.
    auto input_lines = input_file.contents | vws::split('\n') | vws::transform(MakeStringView);
    auto chk = checks.begin();
    auto in = input_lines.begin();
    auto prev = in;

    /// Advance the line and save the previous one.
    auto NextLine = [&] {
        prev = in;
        ++in;
    };

    /// Print the context around a line.
    auto PrintContext = [&](auto it, auto prev_line_it) {
        auto loc = LocationIn(*it, input_file);
        Diag::Note(
            this,
            loc,
            "Expected match here"
        )
            .no_line();

        /// Print only a couple of lines so we don’t dump 2000 lines of output
        /// if the input is long. We start printing one line before the one we
        /// started matching from;
        const auto lc = loc.seek_line_column();
        const auto start = lc.line == 1 ? 1 : lc.line - 1;
        for (auto [i, line] : vws::enumerate(rgs::subrange{prev_line_it, input_lines.end()})) {
            static constexpr usz max_lines = 7;
            if (usz(i) >= max_lines) break;
            fmt::print(stderr, " {: >{}} │ ", start + usz(i), utils::NumberWidth(start + max_lines - 1));
            if (i == 1) fmt::print(stderr, "\033[1;32m{}\n\033[m", line);
            else fmt::print(stderr, "{}\n", line);
        }
    };

    /// Match the input against the checks.
    for (; chk != checks.end() and in != input_lines.end(); ++chk) {
        switch (chk->dir) {
            /// These are not check directives.
            case Directive::Prefix:
            case Directive::Run:
                Unreachable();

            /// Check that any of the following lines matches this line.
            case Directive::CheckAny: {
                /// If the value of this directive is not empty, skip
                /// to the next non-empty line, for better diagnostics.
                if (not chk->check_string.empty()) {
                    while (in != input_lines.end() and (*in).empty()) NextLine();
                    if (in == input_lines.end()) {
                        Diag::Error(
                            this,
                            LocationIn(chk->check_string, check_file),
                            "No match for '{}' in output. All remaining lines are empty",
                            chk->check_string
                        );
                        return;
                    }
                }

                /// Save current line for diagnostics.
                auto save_prev = prev;
                auto save = in;

                /// Perform matching.
                while (in != input_lines.end() and not MatchLine(*in, chk->check_string)) NextLine();

                /// We couldn’t find a line that matches.
                if (in == input_lines.end()) {
                    Diag::Error(
                        this,
                        LocationIn(chk->check_string, check_file),
                        "No match for '{}' in input",
                        chk->check_string
                    );

                    PrintContext(save, save_prev);
                    return;
                }

                NextLine();
            } break;

            case Directive::CheckNext: {
                auto save_prev = prev;
                auto save = in;
                defer { in = ++save; };

                /// This line must match.
                if (not MatchLine(*in, chk->check_string)) {
                    /// Try to see if one of the later lines matches.
                    NextLine();
                    while (in != input_lines.end() and not MatchLine(*in, chk->check_string)) NextLine();
                    if (in != input_lines.end()) {
                        Diag::Error(
                            this,
                            LocationIn(chk->check_string, check_file),
                            "Match for '{}' was not on the next line",
                            chk->check_string
                        );

                        /// Skip any CheckNext directives after this one since they might
                        /// cause bogus errors if we’re already out of sync.
                        while (chk != checks.end() and chk->dir == Directive::CheckNext) chk++;
                    }

                    /// If not, just print a generic error.
                    else {
                        Diag::Error(
                            this,
                            LocationIn(chk->check_string, check_file),
                            "No match for '{}' in input",
                            chk->check_string
                        );
                    }

                    PrintContext(save, save_prev);
                }
            } break;
        }

        /// Take care not to go out of bounds here.
        if (chk == checks.end()) return;
    }

    /// If we have more checks than input lines, we have a problem.
    if (chk != checks.end() and not has_error) Diag::Error(
        this,
        LocationIn(chk->check_string, check_file),
        "String '{}' not found in output",
        chk->check_string
    );
}
