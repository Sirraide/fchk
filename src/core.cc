#include <core.hh>
#include <fmt/color.h>
#include <unordered_map>

#define PCRE2_CODE_UNIT_WIDTH 8
#define PCRE2_STATIC          1
#include <pcre2.h>

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
///  Regex
/// ===========================================================================
/// Exception type to signal redefinition failure.
struct RedefError : std::exception {
    std::string var;
    RedefError(std::string var) : var{std::move(var)} {}
};

Regex::~Regex() noexcept {
    pcre2_code_free(reinterpret_cast<pcre2_code*>(re_ptr));
    pcre2_match_data_free(reinterpret_cast<pcre2_match_data*>(data_ptr));
}

Regex::Regex(std::string_view pattern) {
    int err{};
    usz erroffs{};
    auto expr = pcre2_compile(
        reinterpret_cast<PCRE2_SPTR8>(pattern.data()),
        pattern.size(),
        PCRE2_DOTALL,
        &err,
        &erroffs,
        nullptr
    );

    /// Compilation failed.
    if (not expr) {
        std::string buffer;
        buffer.resize(4'096);
        auto sz = pcre2_get_error_message(
            err,
            reinterpret_cast<PCRE2_UCHAR8*>(buffer.data()),
            buffer.size()
        );

        if (sz == PCRE2_ERROR_BADDATA) Diag::Warning("PCRE error code is invalid");
        else if (sz == PCRE2_ERROR_NOMEMORY) Diag::Warning("PCRE error buffer is too small to accommodate error message");
        else buffer.resize(usz(sz));
        throw Exception("{}", std::move(buffer));
    }

    /// JIT-compile the RE, if possible.
    if (pcre2_jit_compile(expr, PCRE2_JIT_COMPLETE) != 0) {
        pcre2_code_free(expr);
        throw Exception("Failed to JIT compile regex");
    }

    re_ptr = expr;
    data_ptr = pcre2_match_data_create_from_pattern(expr, nullptr);
}

bool Regex::match(std::string_view str, u32 flags = PCRE2_ANCHORED) const noexcept {
    auto re = reinterpret_cast<pcre2_code*>(re_ptr);
    auto data = reinterpret_cast<pcre2_match_data*>(data_ptr);
    int code = pcre2_match(
        re,
        reinterpret_cast<PCRE2_SPTR8>(str.data()),
        str.size(),
        0,
        flags,
        data,
        nullptr
    );
    return code >= 0;
}

EnvironmentRegex::EnvironmentRegex(std::string pattern) : re_str(std::move(pattern)) {
    /// Find all captures defined by this pattern.
    Stream s{re_str};
    for (;;) {
        auto group_name = s.skip_to("?<").skip(2).read_to_or_empty(">");
        if (s.empty()) break;
        defined_captures.emplace(group_name);
    }
}

bool EnvironmentRegex::match(
    std::string_view str,
    utils::StrMap& env,
    u32 flags = 0,
    std::function<void(std::string_view, std::string_view)> capture_visitor
) const {
    std::string subst;

    /// Ensure defined captures don’t overwrite the ENV.
    for (auto& c : defined_captures)
        if (env.contains(c))
            throw RedefError(c);

    /// Substitute named captures that are not defined by this RE and
    /// convert dollar-style captures that are to PCRE2-style '\k<name>'
    /// captures.
    for (Stream s{re_str};;) {
        static constinit std::array<std::string_view, 2> delims{R"(\k<)", "$"sv};
        static const auto IsCaptureGroupName = [](char c) { return std::isalnum(u8(c)) or c == '_'; };
        const auto Add = [&](std::string_view capture, bool escape) {
            if (auto idx = env.find(capture); idx != env.end()) {
                if (not escape) subst += idx->second;
                else subst += fmt::format("\\Q{}\\E", idx->second);
            } else {
                throw Regex::Exception("Undefined capture '{}'", capture);
            }
        };

        /// Get start of next capture group.
        auto fragment = s.read_to_any(delims, true);
        subst += fragment;

        /// An '$' on its own is not a capture group, so we always need
        /// at least two characters (the '$' and another character) for
        /// this to be a capture.
        if (s.size() < 2) break;

        /// PCRE2-style '\k<name>' capture group.
        if (s[0, 1] == "\\") {
            s.skip(R"(\k<)"sv.size());
            auto name = s.read_while(IsCaptureGroupName, true);
            if (not defined_captures.contains(name)) Add(name, false);
            s.skip(">"sv.size());
        }

        /// Dollar capture, w/ optional escaping.
        else {
            bool escape = false;
            s.skip("$"sv.size());
            if (s.at("$")) {
                s.skip("$"sv.size());
                escape = true;
            }

            auto name = s.read_while(IsCaptureGroupName, true);
            if (defined_captures.contains(name)) subst += fmt::format("\\k<{}>", name);
            else Add(name, escape);
        }
    }

    /// Compile the RE and execute it.
    Regex re{subst};
    auto res = re.match(str, flags);
    if (not res) return false;

    /// If the RE matches, extract the captures.
    auto data = reinterpret_cast<pcre2_match_data*>(re.data_ptr);
    auto ov = pcre2_get_ovector_pointer(data);
    for (const auto& name : defined_captures) {
        auto code = pcre2_substring_number_from_name(
            reinterpret_cast<pcre2_code*>(re.re_ptr),
            reinterpret_cast<PCRE2_SPTR>(name.data())
        );

        if (code < 0) throw Regex::Exception("Failed to get capture index for '{}'", name);
        auto start = ov[2 * code];
        auto end = ov[2 * code + 1];
        capture_visitor(name, str.substr(start, end - start));
    }

    return true;
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

    /// Separate error messages w/ an empty line.
    if (ctx) {
        if (kind != Kind::Note and ctx->has_diag) fmt::print(stderr, "\n");
        ctx->has_diag = true;
    }

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

auto Stream::fold_ws() const -> std::string {
    std::string out;
    Stream s{Trim(text)};
    for (;;) {
        out += s.read_to_ws(true);
        if (s.empty()) return out;
        out += ' ';
        s.skip_ws();
    }
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

auto Stream::read_to_any(std::span<SV> delims, bool discard) -> SV {
    auto poss = vws::transform(delims, [&](auto&& d) { return text.find(d); });
    auto min = rgs::min(poss);
    if (min == SV::npos) {
        auto ret = text;
        if (discard) text = "";
        return ret;
    }

    return yield_until(min, discard);
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

auto Stream::skip(usz n) -> Stream& {
    text.remove_prefix(std::min(n, text.size()));
    return *this;
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

auto Stream::skip_to_any(std::span<SV> delims) -> Stream& {
    auto poss = vws::transform(delims, [&](auto&& d) { return text.find(d); });
    auto min = rgs::min(poss);
    if (min == SV::npos) text = "";
    else text.remove_prefix(min);
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

namespace detail {
class Matcher {
    struct Line {
        std::string text;
        Location loc;
    };

    Context* const ctx;
    File& input_file;
    std::vector<Line> input_lines;
    std::vector<Line>::iterator in, prev;
    std::vector<Check>::iterator chk;
    utils::StrMap env;

    /// For providing context around a line in error messages.
    struct LineContext {
        Matcher& m;
        decltype(in) it = m.in, prev = m.prev;
        void print(std::string_view msg) const {
            Diag::Note(m.ctx, it->loc, "{}", msg).no_line();

            /// Print only a couple of lines so we don’t dump 2000 lines of output
            /// if the input is long. We start printing one line before the one we
            /// started matching from;
            const auto lc = it->loc.seek_line_column();
            const auto start = lc.line == 1 ? 1 : lc.line - 1;
            for (auto [i, line] : vws::enumerate(rgs::subrange{prev, m.input_lines.end()})) {
                static constexpr usz max_lines = 7;
                if (usz(i) >= max_lines) break;
                fmt::print(stderr, " {: >{}} │ ", start + usz(i), utils::NumberWidth(start + max_lines - 1));
                if (i == 1) fmt::print(stderr, "\033[1;32m{}\n\033[m", line.text.empty() ? "<empty>" : line.text);
                else fmt::print(stderr, "{}\n", line.text);
            }
        };
    };

    Matcher(Context& ctx, File& input_file) : ctx{&ctx}, input_file{input_file} {
        const auto ProcessLine = [&](auto&& r) -> Line {
            auto sv = std::string_view{&*r.begin(), usz(rgs::distance(r))};
            return {Stream{sv}.fold_ws(), ctx.LocationIn(sv, input_file)};
        };

        /// Split input into lines.
        auto range = input_file.contents | vws::split('\n') | vws::transform(ProcessLine);
        input_lines = {range.begin(), range.end()};
        prev = in = input_lines.begin();
        chk = ctx.checks.begin();
    }

    /// Advance the line and save the previous one.
    void NextLine() {
        prev = in;
        ++in;
    };

    /// Match a line.
    bool MatchLine() {
        const auto DoDef = [&](auto a, auto b) { Define(a, b); };
        if (auto s = std::get_if<std::string>(&chk->data)) return in->text.starts_with(*s);
        else if (auto re = std::get_if<Regex>(&chk->data)) return re->match(in->text);
        else return std::get<EnvironmentRegex>(chk->data).match(in->text, env, 0, DoDef);
    };

    /// Skip a line that matches if the next directive is not a ! directive.
    void SkipMatchingLine() {
        if (
            in == input_lines.end() or
            chk == ctx->checks.end() or
            std::next(chk)->dir == Directive::CheckNot or
            std::next(chk)->dir == Directive::RegexCheckNot or
            std::next(chk)->dir == Directive::InternalCheckNotEmpty
        ) return;
        NextLine();
    }

    void Define(std::string_view key, std::string_view value) {
        if (env.contains(key)) throw RedefError(std::string{key});
        env[std::string{key}] = value;
    }

    /// Issue an error at the current position and print the environment
    /// if the current directive is a regex directive and it is not empty.
    void PrintRegexError(std::string_view msg) {
        Diag::Error(ctx, chk->loc, "{}", msg);
        if (
            chk->dir == Directive::RegexCheckAny or
            chk->dir == Directive::RegexCheckNext or
            chk->dir == Directive::RegexCheckNot
        ) {
            if (env.empty()) return;
            auto env_strs = env | vws::transform([](auto&& p) { return fmt::format("{} = {}", p.first, p.second); });
            Diag::Note(ctx, chk->loc, "With env: [\n    {}\n]\n", fmt::join(env_strs, "\n    ")).no_line();
        }
    }

    /// \brief This function is allowed to throw.
    ///
    /// This function makes use of a variety of exceptions to abort the
    /// matching process since getting access to all the data required
    /// to emit an error from deep within some regex callback is not all
    /// that feasible.
    ///
    /// This function should only ever be called in one place in Match().
    ///
    /// \throw Regex::Exception on an error during Regex compilation.
    /// \throw RedefError when attempting to redefine a variable.
    void Step() {
        LineContext context{*this};
        switch (chk->dir) {
            /// These should no longer exist here.
            case Directive::Prefix:
            case Directive::Run:
                Unreachable();

            case Directive::Define: {
                auto& line = std::get<std::string>(chk->data);
                Stream s{line};
                auto name = s.read_to_ws(true);
                auto value = *s.skip_ws();
                Define(name, value);
            } break;

            case Directive::Undefine: {
                auto& var = std::get<std::string>(chk->data);
                if (var == "*") env.clear();
                else if (auto it = env.find(var); it != env.end()) env.erase(it);
                else Diag::Warning(ctx, chk->loc, "Variable '{}' is not defined", var);
            } break;

            case Directive::InternalCheckEmpty: {
                while (in != input_lines.end() and not in->text.empty()) NextLine();
                if (in == input_lines.end()) {
                    Diag::Error(ctx, chk->loc, "Expected empty line");
                    context.print("Started matching here");
                }
            } break;

            case Directive::InternalCheckNextEmpty: {
                if (in->text.empty()) NextLine();
                else {
                    Diag::Error(ctx, chk->loc, "Expected next line to be empty");
                    context.print("Here");
                }
            } break;

            case Directive::InternalCheckNotEmpty: {
                if (not in->text.empty()) NextLine();
                else {
                    Diag::Error(ctx, chk->loc, "Expected line to not be empty");
                    context.print("Here");
                }
            } break;

            /// Check that any of the following lines matches.
            case Directive::CheckAny:
            case Directive::RegexCheckAny: {
                /// Perform matching.
                while (in != input_lines.end() and not MatchLine()) NextLine();

                /// We couldn’t find a line that matches.
                if (in == input_lines.end()) {
                    PrintRegexError("Expected string not found in input");
                    context.print("Started matching here");
                    return;
                }

                /// Skip the matching line.
                SkipMatchingLine();
            } break;

            /// Check that this line matches.
            case Directive::CheckNext:
            case Directive::RegexCheckNext: {
                /// We may look ahead a few lines below so we can issue a better
                /// error message; make sure to reset the state in case we do that.
                const auto save = in;
                defer {
                    in = save;
                    prev = in == input_lines.begin() ? in : std::prev(in);
                    SkipMatchingLine();
                };

                /// This line must match.
                if (not MatchLine()) {
                    /// Try to see if one of the later lines matches.
                    NextLine();
                    while (in != input_lines.end() and not MatchLine()) NextLine();

                    /// If the input was not on the next line, skip any CheckNext directives
                    /// after this one since they might cause bogus errors if we’re already out
                    /// of sync.
                    if (in != input_lines.end()) {
                        Diag::Error(ctx, chk->loc, "Line does not match expected string");
                        while (chk != ctx->checks.end() and chk->dir == Directive::CheckNext) chk++;
                    }

                    /// If not, just print a generic error.
                    else { PrintRegexError("Expected string not found in input"); }
                    /// TODO: Print environment.
                    context.print("Expected match here");
                }
            } break;

            /// Check that this line does not match.
            case Directive::CheckNot: {
                if (in->text.contains(std::get<std::string>(chk->data))) {
                    Diag::Error(ctx, chk->loc, "Input contains prohibited string");
                    context.print("In this line");
                }

                SkipMatchingLine();
            } break;

            /// Check that this line does not match a regular expression.
            case Directive::RegexCheckNot: {
                if (auto re = std::get_if<Regex>(&chk->data)) {
                    if (re->match(in->text, 0)) {
                        Diag::Error(ctx, chk->loc, "Input contains prohibited string");
                        context.print("In this line");
                    }
                }

                else if (auto env_re = std::get_if<EnvironmentRegex>(&chk->data)) {
                    if (env_re->match(in->text, env, 0, [&](auto a, auto b) { Define(a, b); })) {
                        PrintRegexError("Input contains prohibited string");
                        context.print("In this line");
                    }
                }

                else {
                    SkipMatchingLine();
                }
            } break;
        }
    }

    void Match() {
        /// Match the input against the checks.
        for (; chk != ctx->checks.end() and in != input_lines.end(); ++chk) {
            try {
                Step();
            } catch (const Regex::Exception& e) {
                Diag::Error(ctx, chk->loc, "Invalid regular expression: {}", e.message);
            } catch (const RedefError& e) {
                Diag::Error(
                    ctx,
                    chk->loc,
                    "'{}' is already defined. Use 'u {}' to undefine it.",
                    e.var,
                    e.var
                );
            }

            /// Take care not to go out of bounds here.
            if (chk == ctx->checks.end()) return;
        }

        /// If we have more checks than input lines, we have a problem.
        if (chk != ctx->checks.end() and not ctx->has_error) Diag::Error(
            ctx,
            chk->loc,
            "End of file reached looking for string"
        );
    }

public:
    static void Match(Context& ctx, File& input_file) {
        Matcher{ctx, input_file}.Match();
    }
};
} // namespace detail

int Context::Run() {
    static std::unordered_map<std::string_view, Directive> name_directive_map{
        {DirectiveNames[+Directive::CheckAny], Directive::CheckAny},
        {DirectiveNames[+Directive::CheckNext], Directive::CheckNext},
        {DirectiveNames[+Directive::CheckNot], Directive::CheckNot},
        {DirectiveNames[+Directive::RegexCheckAny], Directive::RegexCheckAny},
        {DirectiveNames[+Directive::RegexCheckNext], Directive::RegexCheckNext},
        {DirectiveNames[+Directive::RegexCheckNot], Directive::RegexCheckNot},
        {DirectiveNames[+Directive::Define], Directive::Define},
        {DirectiveNames[+Directive::Undefine], Directive::Undefine},
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

        /// Add the check.
        const auto d = it->second;
        const auto loc = LocationIn(value, check_file);
        const auto Add = [&](auto&& val, Directive d) { checks.emplace_back(d, std::forward<decltype(val)>(val), loc); };
        switch (it->second) {
            case Directive::Prefix:
            case Directive::Run:
            case Directive::InternalCheckEmpty:
            case Directive::InternalCheckNextEmpty:
            case Directive::InternalCheckNotEmpty:
                Unreachable();

            case Directive::Define:
            case Directive::Undefine:
            _default:
                Add(Stream{value}.fold_ws(), d);
                break;

            /// Optimise for empty lines.
            case Directive::CheckAny:
                if (value.empty()) Add(Check::Data{}, Directive::InternalCheckEmpty);
                else goto _default;
                break;

            case Directive::CheckNext:
                if (value.empty()) Add(Check::Data{}, Directive::InternalCheckNextEmpty);
                else goto _default;
                break;

            case Directive::CheckNot:
                if (value.empty()) Add(Check::Data{}, Directive::InternalCheckNotEmpty);
                else goto _default;
                break;

            /// Take care to handle regex directives.
            case Directive::RegexCheckAny:
            case Directive::RegexCheckNext:
            case Directive::RegexCheckNot: {
                /// Regex constructor may throw so we don’t have to check for errors
                /// everywhere we use a regular expression since there is no point in
                /// trying to match anything with faulty regular expressions.
                try {
                    /// Construct an environment regex if captures are used.
                    static constinit std::array<std::string_view, 3> delims{"?<"sv, R"(\k<)", "$"sv};
                    if (Stream{value}.skip_to_any(delims).size() >= 2) Add(EnvironmentRegex{Stream{value}.fold_ws()}, d);
                    else Add(Regex{Stream{value}.fold_ws()}, d);
                } catch (const Regex::Exception& e) {
                    Diag::Error(
                        this,
                        LocationIn(value, check_file),
                        "Invalid regular expression: {}",
                        e.message
                    );
                }
            } break;
        }
    }

    /// Don’t even bother matching if there was an error.
    if (has_error) return 1;

    /// Can’t check anything w/ no directives.
    if (run_directives.empty()) Diag::Fatal(
        "No {} directives found in check file!",
        DirectiveNames[+Directive::Run]
    );

    /// Dew it.
    for (auto rd : run_directives) RunTest(rd);
    return has_error ? 1 : 0;
}

void Context::RunTest(std::string_view test) {
    /// Substitute occurrences of `%s` with the file name.
    auto cmd = std::string{test};
    utils::ReplaceAll(cmd, "%s", check_file.path.string());

    /// Run the command and get its output.
    auto pipe = ::popen(cmd.c_str(), "r");
    if (not pipe) Diag::Fatal("Failed to run command '{}': {}", cmd, std::strerror(errno));
    File input_file{utils::Drain(pipe), "<input>"};
    ::pclose(pipe);
    detail::Matcher::Match(*this, input_file);
}
