#include <core.hh>
#include <fmt/color.h>
#include <unordered_map>

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
    return pos + len <= file->contents.size() and is_valid();
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

    /// Print the line up to the start of the location, the range in the right
    /// colour, and the rest of the line.
    fmt::print(stderr, " {} | {}", line, before);
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

auto Stream::read_to(SV delim) -> SV {
    auto pos = text.find(delim);
    if (pos == SV::npos) return "";
    return text.substr(0, pos);
}

auto Stream::read_to_or_end(SV delim) -> SV {
    auto pos = text.find(delim);
    if (pos == SV::npos) return text;
    return text.substr(0, pos);
}

auto Stream::read_to_any(SV delims) -> SV {
    auto pos = text.find_first_of(delims);
    if (pos == SV::npos) return "";
    return text.substr(0, pos);
}

auto Stream::read_to_ws() -> SV {
    return read_to_any(ws);
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
    return skip_to_any(ws);
}

auto Stream::skip_ws() -> Stream& {
    auto pos = text.find_first_not_of(ws);
    if (pos == SV::npos) text = "";
    else text.remove_prefix(pos);
    return *this;
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

        if (prefix.empty()) die(
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

        checks.emplace_back(it->second, value);
    }

    /// Print them.
    for (auto& chk : checks) Diag::Note(
        this,
        LocationIn(chk.check_string, check_file),
        "Found {} directive: {}",
        DirectiveNames[+chk.dir],
        chk.check_string
    );

    return 0;
}
