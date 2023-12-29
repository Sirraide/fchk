#include <core.hh>
#include <fmt/color.h>
#include <unordered_map>

#define PCRE2_CODE_UNIT_WIDTH 8
#define PCRE2_STATIC          1
#include <pcre2.h>

#ifdef _WIN32
#    define NOMINMAX
#    include <Windows.h>
#endif

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

#ifdef _WIN32
/// Get error message from last error on Windows.
auto GetWindowsError() -> std::string {
    DWORD err = GetLastError();
    LPSTR buffer{};
    auto sz = FormatMessageA(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        nullptr,
        err,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        reinterpret_cast<LPSTR>(&buffer),
        0,
        nullptr
    );

    if (sz == 0) return fmt::format("Unknown error {}", err);
    std::string msg{buffer, sz};
    LocalFree(buffer);
    return msg;
}
#endif

auto GetProcessOutput(std::string_view cmd) -> std::string {
#ifndef _WIN32
    auto pipe = ::popen(cmd.data(), "r");
    if (not pipe) {
        Diag::Error("Failed to run command '{}': {}", cmd, std::strerror(errno));
        std::exit(1);
    }

    auto output = utils::Drain(pipe);
    auto code = ::pclose(pipe);
    if (not WIFEXITED(code)) {
        Diag::Error("Command '{}' exited abnormally", cmd);
        std::exit(1);
    }

    if (WEXITSTATUS(code) != 0) {
        Diag::Error("Command '{}' exited with status {}", cmd, WEXITSTATUS(code));
        std::exit(1);
    }

    return output;
#else
    HANDLE pipe_read{}, pipe_write{};
    SECURITY_ATTRIBUTES sa{};
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = nullptr;

    /// Create the pipe
    if (not CreatePipe(&pipe_read, &pipe_write, &sa, 0))
        Diag::Fatal("Failed to create pipe: {}", GetWindowsError());

    /// Create the child process.
    STARTUPINFO si{};
    PROCESS_INFORMATION pi{};
    si.cb = sizeof(STARTUPINFO);
    si.hStdError = pipe_write;
    si.hStdOutput = pipe_write;
    si.dwFlags |= STARTF_USESTDHANDLES;

    auto command = fmt::format("cmd /c {}", cmd);
    if (
        not CreateProcess(
            nullptr,
            command.data(),
            nullptr,
            nullptr,
            TRUE,
            0,
            nullptr,
            nullptr,
            &si,
            &pi
        )
    ) {
        Diag::Error("Failed to create process: {}", GetWindowsError());
        std::exit(1);
    }

    /// Close the write end of the pipe.
    CloseHandle(pipe_write);

    /// Read the output.
    std::string output;
    for (;;) {
        static constexpr usz bufsize = 4'096;
        output.resize(output.size() + bufsize);
        DWORD read{};
        if (not ReadFile(pipe_read, output.data() + output.size() - bufsize, bufsize, &read, nullptr)) {
            if (GetLastError() == ERROR_BROKEN_PIPE) break;
            Diag::Fatal("Failed to read from pipe: {}", GetWindowsError());
        }
        if (read == 0) break;
        output.resize(output.size() - (bufsize - read));
    }

    /// Close the read end of the pipe.
    CloseHandle(pipe_read);

    /// Wait for the process to exit.
    WaitForSingleObject(pi.hProcess, INFINITE);

    /// Check its exit code.
    DWORD exit_code{};
    if (GetExitCodeProcess(pi.hProcess, &exit_code) == FALSE)
        Diag::Fatal("Failed to get exit code: {}", GetWindowsError());

    if (exit_code != 0) {
        Diag::Error("Command '{}' exited with status {}", cmd, exit_code);
        std::exit(1);
    }

    /// Close the process and thread handles.
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    /// Done!
    return output;
#endif
}

} // namespace

Context::Context(
    std::string check,
    fs::path check_name,
    std::string prefix,
    utils::Map<std::string, bool> pragmas,
    std::unordered_set<char> literal_chars,
    std::span<const std::string> defines,
    bool abort_on_error,
    bool verbose
) : check_file{std::move(check), std::move(check_name)},
    default_pragmas(std::move(pragmas)),
    default_literal_chars(std::move(literal_chars)),
    abort_on_error(abort_on_error),
    verbose(verbose) {
    /// Initialise known pragmas.
    if (not default_pragmas.contains("re")) default_pragmas["re"] = false;
    if (not default_pragmas.contains("nocap")) default_pragmas["nocap"] = false;
    if (not default_pragmas.contains("captype")) default_pragmas["captype"] = false;

    /// Create initial state.
    CreatePrefixState(prefix);

    /// Save definitions.
    for (auto& d : defines) {
        auto eq = d.find('=');
        if (eq == std::string::npos) Diag::Fatal("Syntax of '-D' option is '-D name=value'", d);
        definitions["%" + d.substr(0, eq)] = d.substr(eq + 1);
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
    pcre2_code_free(static_cast<pcre2_code*>(re_ptr));
    pcre2_match_data_free(static_cast<pcre2_match_data*>(data_ptr));
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

    /*
        /// JIT-compile the RE, if possible.
        if (pcre2_jit_compile(expr, PCRE2_JIT_COMPLETE) != 0) {
            pcre2_code_free(expr);
            throw Exception("Failed to JIT compile regex");
        }
    */

    raw = pattern;
    re_ptr = expr;
    data_ptr = pcre2_match_data_create_from_pattern(expr, nullptr);
}

bool Regex::match(std::string_view str, u32 flags = 0) const noexcept {
    auto re = static_cast<pcre2_code*>(re_ptr);
    auto data = static_cast<pcre2_match_data*>(data_ptr);
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

EnvironmentRegex::EnvironmentRegex(
    std::string pattern,
    std::unordered_set<char> literal_chars,
    bool captype
) : re_str(std::move(pattern)),
    literal_chars(std::move(literal_chars)) {
    /// Preprocessing step for typed captures.
    if (captype) {
        std::string processed;
        Stream s{re_str};
        for (;;) {
            processed += s.read_to("$", true);
            Stream saved = s;
            s.skip(1);
            if (s.empty()) {
                processed += *saved;
                break;
            }

            /// Find ":".
            auto capture = s.read_while([](char c) { return std::isalnum(u8(c)) or c == '_'; }, true);

            /// Got one.
            if (s.at(":") and (s.skip(1), not s.empty() and not s.at_any(" \t\n\r\f\v"))) {
                auto type = s.read_while([](char c) { return std::isalnum(u8(c)) or c == '_'; }, true);
                processed += fmt::format("(?<{}>${})", capture, type);
            }

            /// Regular capture.
            else {
                processed += saved.read(saved.size() - s.size(), true);
                s = saved;
            }
        }
        re_str = std::move(processed);
    }

    /// Find all captures defined by this pattern.
    Stream s{re_str};
    for (;;) {
        auto group_name = s.skip_to("?<").skip(2).read_to_or_empty(">");
        if (s.empty()) break;
        defined_captures.emplace(group_name);
    }
}

auto EnvironmentRegex::substitute_vars(const Environment& env) -> std::string {
    std::string subst;

    /// Substitute named captures that are not defined by this RE and
    /// convert dollar-style captures that are to PCRE2-style '\k<name>'
    /// captures.
    for (Stream s{re_str};;) {
        static constinit std::array<std::string_view, 2> delims{R"(\k<)", "$"sv};
        static const auto IsCaptureGroupName = [](char c) { return std::isalnum(u8(c)) or c == '_'; };
        const auto Add = [&](std::string_view capture, bool escape) {
            if (auto var = rgs::find(env, capture, &EnvEntry::name); var != env.end()) {
                /// Always escape literal variables.
                if (not escape and not var->literal) subst += var->value;
                else subst += fmt::format("\\Q{}\\E", var->value);
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
        if (s.front() == '\\') {
            s.skip(R"(\k<)"sv.size());
            auto name = s.read_while(IsCaptureGroupName, true);
            if (defined_captures.contains(name)) subst += fmt::format("\\k<{}>", name);
            else Add(name, false);
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

    return subst;
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
auto Stream::substr(usz start, usz end) const -> SV {
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

auto Stream::read(usz elems, bool discard) -> SV {
    return yield_until(elems, discard);
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
    [[maybe_unused]] File& input_file;
    std::span<Check> checks;
    std::vector<Line> input_lines;
    std::vector<Line>::iterator in, prev;
    std::span<Check>::iterator chk;
    Environment env;

    /// Whether we’re in a local environment.
    bool in_local_env = false;

    /// For providing context around a line in error messages.
    struct LineContext {
        Matcher& m;
        decltype(in) it = m.in, prev = m.prev;

        void print(std::string_view msg) const {
            /// Print the message w/o the code line.
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
                if (i == 1 - (lc.line == 1)) fmt::print(stderr, "\033[1;32m{}\n\033[m", line.text.empty() ? "<empty>" : line.text);
                else fmt::print(stderr, "{}\n", line.text);
            }
        };

        void restore() {
            m.in = it;
            m.prev = prev;
        }
    };

    Matcher(Context& ctx, File& input_file, std::span<Check> checks) : ctx{&ctx}, input_file{input_file}, checks{checks} {
        const auto ProcessLine = [&](auto&& r) -> Line {
            auto sv = std::string_view{&*r.begin(), usz(rgs::distance(r))};
            return {Stream{sv}.fold_ws(), ctx.LocationIn(sv, input_file)};
        };

        /// Split input into lines.
        auto range = input_file.contents | vws::split('\n') | vws::transform(ProcessLine);
        input_lines = {range.begin(), range.end()};
        prev = in = input_lines.begin();
        chk = this->checks.begin();
    }

    /// Match a regular expression that uses the environment.
    bool MatchEnvRegex(EnvironmentRegex& re) {
        /// Ensure defined captures don’t overwrite the ENV.
        for (auto& c : re.defined_captures)
            if (rgs::contains(env, c, &EnvEntry::name))
                throw RedefError(c);

        /// Compile the RE and execute it.
        Regex expr{re.substitute_vars(env)};
        auto res = expr.match(in->text, 0);
        if (not res) return false;

        /// If the RE matches, extract the captures.
        auto data = reinterpret_cast<pcre2_match_data*>(expr.data());
        auto ov = pcre2_get_ovector_pointer(data);
        for (const auto& name : re.defined_captures) {
            auto code = pcre2_substring_number_from_name(
                reinterpret_cast<pcre2_code*>(expr.ptr()),
                reinterpret_cast<PCRE2_SPTR>(name.data())
            );

            if (code < 0) throw Regex::Exception("Failed to get capture index for '{}'", name);
            auto start = ov[2 * code];
            auto end = ov[2 * code + 1];
            Define(name, in->text.substr(start, end - start), true);
        }

        return true;
    }

    /// Advance the line and save the previous one.
    void NextLine() {
        prev = in;
        ++in;
    };

    /// Match a line.
    bool MatchLine() {
        if (auto s = std::get_if<std::string>(&chk->data)) return in->text.contains(*s);
        else if (auto re = std::get_if<Regex>(&chk->data)) return re->match(in->text);
        else return MatchEnvRegex(std::get<EnvironmentRegex>(chk->data));
    };

    /// Skip a line if the next directive is not a directive
    /// that does not advance the line pointer.
    void AdvanceLineAfterCheck() {
        if (
            in == input_lines.end() or
            chk == checks.end() or
            std::next(chk) == checks.end() or
            std::next(chk)->dir == Directive::CheckNotSame or
            std::next(chk)->dir == Directive::RegexCheckNotSame
        ) return;
        NextLine();
    }

    void Define(std::string_view key, std::string_view value, bool capture) {
        if (rgs::contains(env, key, &EnvEntry::name)) throw RedefError(std::string{key});
        env.emplace_back(std::string{key}, std::string{value}, capture, in_local_env);
    }

    /// Issue an error at the current position and print the environment
    /// if the current directive is a regex directive and it is not empty.
    void PrintRegexError(std::string_view msg) {
        Diag::Error(ctx, chk->loc, "{}", msg);
        if (
            chk->dir == Directive::RegexCheckAny or
            chk->dir == Directive::RegexCheckNext or
            chk->dir == Directive::RegexCheckNotSame or
            chk->dir == Directive::RegexCheckNotAny or
            chk->dir == Directive::RegexCheckNotNext
        ) {
            if (env.empty()) return;
            if (ctx->verbose) {
                auto env_strs = env | vws::transform([](auto&& p) { return fmt::format("{} = {}", p.name, p.value); });
                Diag::Note(ctx, chk->loc, "With env: [\n    {}\n]\n", fmt::join(env_strs, "\n    ")).no_line();
            }

            /// Print expansion of regex that contains captures.
            if (auto re = std::get_if<EnvironmentRegex>(&chk->data)) {
                Diag::Note(
                    ctx,
                    chk->loc,
                    "Expands to: {}",
                    re->substitute_vars(env)
                )
                    .no_line();

                /// Separate from the note after it.
                fmt::print(stderr, "\n");
            }
        }
    }

    /// Check if we’re at any kind of CheckNext directive and skip
    /// them; note that Match() will skip the current check anyway,
    /// so keep skipping until the *next* directive is not a CheckNext
    /// directive.
    void SkipCheckNextDirs() {
        while (chk < std::prev(checks.end())) {
            switch (std::next(chk)->dir) {
                default: return;
                case Directive::CheckNext:
                case Directive::RegexCheckNext:
                case Directive::CheckNotNext:
                case Directive::RegexCheckNotNext:
                    chk++;
            }
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
            case Directive::Pragma:
                Unreachable();

            case Directive::Begin: {
                /// Yeet everything defined after the last definition point.
                std::erase_if(env, &EnvEntry::local);
                in_local_env = true;
            } break;

            case Directive::Define: {
                auto& line = std::get<std::string>(chk->data);
                Stream s{line};
                auto name = s.read_to_ws(true);
                auto value = *s.skip_ws();
                Define(name, value, false);
            } break;

            case Directive::Undefine: {
                auto& var = std::get<std::string>(chk->data);
                if (var == "*") env.clear();
                else if (auto it = rgs::find(env, var, &EnvEntry::name); it != env.end()) env.erase(it);
                else Diag::Warning(ctx, chk->loc, "Variable '{}' is not defined", var);
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

                AdvanceLineAfterCheck();
            } break;

            /// Check that none of the following lines match.
            case Directive::CheckNotAny:
            case Directive::RegexCheckNotAny: {
                /// We’ll have to read ahead to the end of the file, so
                /// restore the context when we’re done with that and skip
                /// just one line.
                defer { context.restore(); };

                /// Check that the string does not occur in the file.
                while (in != input_lines.end() and not MatchLine()) NextLine();
                if (in != input_lines.end()) {
                    PrintRegexError("Input contains prohibited string");
                    context.print("Started matching here");
                }
            } break;

            /// Check that the next line does not match.
            case Directive::CheckNotNext:
            case Directive::RegexCheckNotNext: {
                if (MatchLine()) {
                    PrintRegexError("Input contains prohibited string");
                    context.print("Here");

                    /// Skip any CheckNext directives after this one since they might
                    /// cause bogus errors if we’re already out of sync.
                    SkipCheckNextDirs();
                }

                AdvanceLineAfterCheck();
            } break;

            /// Check that this line matches.
            case Directive::CheckNext:
            case Directive::RegexCheckNext: {
                /// This line must match.
                if (not MatchLine()) {
                    PrintRegexError("Line does not match expected string");
                    context.print("Expected match here");

                    /// Skip any CheckNext directives after this one since they might
                    /// cause bogus errors if we’re already out of sync.
                    SkipCheckNextDirs();
                }

                AdvanceLineAfterCheck();
            } break;

            /// Check that this line does not match.
            case Directive::CheckNotSame: {
                if (in->text.contains(std::get<std::string>(chk->data))) {
                    Diag::Error(ctx, chk->loc, "Input contains prohibited string");
                    context.print("In this line");
                }

                AdvanceLineAfterCheck();
            } break;

            /// Check that this line does not match a regular expression.
            case Directive::RegexCheckNotSame: {
                if (auto re = std::get_if<Regex>(&chk->data)) {
                    if (re->match(in->text, 0)) {
                        Diag::Error(ctx, chk->loc, "Input contains prohibited string");
                        context.print("In this line");
                    }
                }

                else if (auto env_re = std::get_if<EnvironmentRegex>(&chk->data)) {
                    if (MatchEnvRegex(*env_re)) {
                        PrintRegexError("Input contains prohibited string");
                        context.print("In this line");
                    }
                }

                else {
                    AdvanceLineAfterCheck();
                }
            } break;
        }
    }

    void Match() {
        /// Match the input against the checks.
        for (; chk != checks.end() and in != input_lines.end(); ++chk) {
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

            /// Halt on error if requested.
            if (ctx->has_error and ctx->abort_on_error) std::exit(1);

            /// Take care not to go out of bounds here.
            if (chk == checks.end()) return;
        }

        /// If we have more checks than input lines, we have a problem,
        /// except if the checks are negative checks, which we can simply
        /// discard.
        while (chk != checks.end() and chk->is_negative_check()) ++chk;
        if (chk != checks.end() and not ctx->has_error) Diag::Error(
            ctx,
            chk->loc,
            "End of file reached looking for string"
        );
    }

public:
    static void Match(Context& ctx, File& input_file, std::span<Check> checks) {
        Matcher{ctx, input_file, checks}.Match();
    }
};

static std::unordered_map<std::string_view, Directive> NameDirectiveMap{
    {DirectiveNames[+Directive::CheckAny], Directive::CheckAny},
    {DirectiveNames[+Directive::CheckNext], Directive::CheckNext},
    {DirectiveNames[+Directive::CheckNotSame], Directive::CheckNotSame},
    {DirectiveNames[+Directive::CheckNotAny], Directive::CheckNotAny},
    {DirectiveNames[+Directive::CheckNotNext], Directive::CheckNotNext},
    {DirectiveNames[+Directive::RegexCheckAny], Directive::RegexCheckAny},
    {DirectiveNames[+Directive::RegexCheckNext], Directive::RegexCheckNext},
    {DirectiveNames[+Directive::RegexCheckNotSame], Directive::RegexCheckNotSame},
    {DirectiveNames[+Directive::RegexCheckNotAny], Directive::RegexCheckNotAny},
    {DirectiveNames[+Directive::RegexCheckNotNext], Directive::RegexCheckNotNext},
    {DirectiveNames[+Directive::Begin], Directive::Begin},
    {DirectiveNames[+Directive::Define], Directive::Define},
    {DirectiveNames[+Directive::Undefine], Directive::Undefine},
    {DirectiveNames[+Directive::Pragma], Directive::Pragma},
    {DirectiveNames[+Directive::Prefix], Directive::Prefix},
    {DirectiveNames[+Directive::Run], Directive::Run},
};

const auto RunWithPrefixDirectiveNameStart = fmt::format("{}[", DirectiveNames[+Directive::Run]);

} // namespace detail

void Context::CollectDirectives(PrefixState& state) {
    Stream chfile{check_file.contents};

    /// Read a directive’s argument, if any.
    auto ReadDirectiveArg = [&] { return chfile.at("\n") ? "" : Trim(chfile.skip_to_ws().skip_ws().read_to("\n")); };

    /// Find all directives in the file.
    while (not chfile.empty()) {
        chfile.skip_to(state.prefix).skip(state.prefix.size()).skip_ws();

        /// Handle run directives.
        if (chfile.at(detail::RunWithPrefixDirectiveNameStart)) {
            auto prefix = Trim(chfile.read_to_any("]\r\n", true));
            if (not chfile.at("]")) continue;
            prefix.remove_prefix(detail::RunWithPrefixDirectiveNameStart.size());

            /// If there currently is no state for this prefix, create it.
            PrefixState* new_state;
            if (
                auto it = rgs::find(states_by_prefix, prefix, &PrefixState::prefix);
                it == states_by_prefix.end()
            ) {
                new_state = CreatePrefixState(std::string{prefix});

                /// Collect all directives for the new prefix.
                CollectDirectives(*new_state);
            } else {
                new_state = &*it;
            }

            /// Add a run directive.
            run_directives.emplace_back(ReadDirectiveArg(), new_state);
            continue;
        }

        /// Read directive.
        auto dir = Trim(chfile.read_to_any(" \t\v\f\r\n", true));
        if (dir.empty()) continue;

        /// Check if this really is a directive.
        auto it = detail::NameDirectiveMap.find(dir);
        if (it == detail::NameDirectiveMap.end()) continue;
        auto value = ReadDirectiveArg();

        /// Handle spurious prefix directives
        if (it->second == Directive::Prefix) {
            /// Overriding the prefix is not allowed anymore.
            if (state.prefix != value) Diag::Warning(
                this,
                LocationIn(dir, check_file),
                "Conflicting prefix directive ignored (current prefix is '{}')",
                state.prefix
            );
            continue;
        }

        /// Regular run directive.
        if (it->second == Directive::Run) {
            run_directives.emplace_back(value, &state);
            continue;
        }

        /// Helper to abbreviate adding a check.
        Directive d = it->second;
        const auto loc = LocationIn(value, check_file);
        const auto Add = [&](auto&& val, Directive d) { state.checks.emplace_back(d, std::forward<decltype(val)>(val), loc); };

        /// Helper to apply the 'p nocap' and 'p lit' pragmas to a string.
        auto SubstituteNoCapAndLiterals = [&](std::string& expr) {
            /// If someone is using 'p (no)lit' on '()', then that’s
            /// their problem; we warn about that already, so don’t
            /// bother checking that and just handle 'p nocap' here.
            ///
            /// Furthermore, ignore unmatched parens as the regex
            /// engine will error over that anyway.
            if (state.pragmas["nocap"]) {
                /// We need to 1. not escape '(' and ')' if the '('
                /// is followed by '?', and 2. make sure we match
                /// the closing ')' correctly.
                usz i = 0;
                auto Escape = [&]<bool top_level = false>(auto& Self) {
                    while (i < expr.size()) {
                        /// Skip to next open paren, or closing paren, if
                        /// we’ve seen an open paren.
                        i = expr.find_first_of("()", i);
                        if (i == std::string::npos) return;

                        /// At a closing paren, let ur caller handle this
                        /// if we’re not at the top level; if we are, just
                        /// escape it.
                        if (expr[i] == ')') {
                            if (not top_level) return;
                            expr.insert(i, "\\");
                            i += 2;
                            continue;
                        }

                        /// Escape any opening parens at the end.
                        if (i == expr.size() - 1) {
                            expr.insert(i, "\\");
                            i += "\\("sv.size();
                            return;
                        }

                        /// If the next character is a '?', recurse to handle
                        /// nested parens, and leave the matching closing paren
                        /// as is.
                        if (expr[i + 1] == '?') {
                            i++;
                            Self(Self);
                            if (i < expr.size() and expr[i] == ')') i++;
                            continue;
                        }

                        /// Otherwise, escape the paren, recurse to take care of
                        /// nested parens, and escape the corresponding closing
                        /// paren, if there is one.
                        expr.insert(i, "\\");
                        i += "\\("sv.size();
                        Self(Self);
                        if (i < expr.size() and expr[i] == ')') {
                            expr.insert(i, "\\");
                            i += "\\)"sv.size();
                        }
                    }
                };

                /// Yes, this is how you call a templated lambda.
                Escape.template operator()<true>(Escape);
            }

            /// Escape dots if the corresponding pragma is set.
            for (auto c : state.literal_chars) utils::ReplaceAll(
                expr,
                std::string_view{&c, 1},
                fmt::format("\\{}", c)
            );
        };

        /// Handle directive.
        switch (it->second) {
            case Directive::Prefix:
            case Directive::Run:
                Unreachable();

            case Directive::CheckAny:
            case Directive::CheckNext:
            case Directive::CheckNotSame:
            case Directive::CheckNotAny:
            case Directive::CheckNotNext:
                if (state.pragmas["re"]) {
                    d = DirectiveToRegexDirective[usz(d)];
                    goto regex_directive;
                }

                Add(Stream{value}.fold_ws(), d);
                break;

            case Directive::Undefine:
                Add(Stream{value}.fold_ws(), d);
                break;

            case Directive::Begin:
                Add("", d);
                break;

            /// Value of a define must honour literals at definition time.
            case Directive::Define: {
                auto text = Stream{value}.fold_ws();
                SubstituteNoCapAndLiterals(text);
                Add(text, d);
            } break;

            /// Handle pragmas.
            case Directive::Pragma: {
                if (value.empty()) Diag::Fatal("'p' directive requires an argument");
                auto s = Stream{value};
                auto name = s.read_to_ws(true);
                auto arg = s.skip_ws().read_to_ws(true);

                /// Some pragmas require special handling.
                if (name == "lit" or name == "nolit") {
                    /// No-op if no chars were provided
                    if (arg.empty()) {
                        Diag::Warning(
                            this,
                            LocationIn(arg, check_file),
                            "Empty '{}' pragma ignored",
                            name
                        );

                        continue;
                    }

                    /// Making backslashes literal would break things.
                    if (name == "lit" and arg.contains('\\')) Diag::Error(
                        this,
                        LocationIn(arg, check_file),
                        "Escape character '\\' cannot be made literal",
                        name
                    );

                    /// Warn about '(' and ')'.
                    if (arg.contains('(') or arg.contains(')')) Diag::Warning(
                        this,
                        LocationIn(arg, check_file),
                        "Prefer using 'p nocap' over making '(' or ')' (not) literal "
                        "as the latter can cause the regex engine to error."
                    );

                    /// Tell the user that `off` isn’t supported for this pragma. Since
                    /// 'o' and 'f' are not metacharacters anyway, passing 'off' to this
                    /// wouldn’t to anything even if we accepted it.
                    if (arg == "off") {
                        Diag::Warning(
                            this,
                            LocationIn(arg, check_file),
                            "Syntax of '{}' pragma is 'p {} <chars>'",
                            name,
                            name
                        );

                        continue;
                    }

                    /// Add/remove the literal chars.
                    if (name == "lit") {
                        /// We don’t error on making 'Q' or 'E' literal, but since '\Q'
                        /// and '\E' are used for escaping, they themselves should not
                        /// be escaped, ever. To simplify things, we just ignore letters
                        /// and numbers altogether here.
                        for (auto c : arg)
                            if (not std::isalnum(u8(c)))
                                state.literal_chars.insert(c);
                    } else {
                        std::erase_if(state.literal_chars, [&](auto c) { return arg.contains(c); });
                    }
                }

                /// Other pragmas are simple boolean flags.
                else {
                    /// Ignore unknown pragmas.
                    if (not state.pragmas.contains(name)) {
                        Diag::Warning(
                            this,
                            LocationIn(name, check_file),
                            "Unknown pragma ignored"
                        );

                        continue;
                    }

                    /// Pragmas take an optional ‘off’ parameter.
                    if (not arg.empty() and arg != "off") Diag::Warning(
                        this,
                        LocationIn(arg, check_file),
                        "Unknown pragma argument ignored"
                    );

                    /// Set the pragma.
                    state.pragmas[std::string{name}] = arg != "off";
                }

                /// Warn about junk.
                if (not s.skip_ws().empty()) Diag::Warning(
                    this,
                    LocationIn(*s, check_file),
                    "Junk at end of pragma ignored"
                );
            } break;

            /// Take care to handle regex directives.
            case Directive::RegexCheckAny:
            case Directive::RegexCheckNext:
            case Directive::RegexCheckNotSame:
            case Directive::RegexCheckNotAny:
            case Directive::RegexCheckNotNext:
            regex_directive: {
                /// Regex constructor may throw so we don’t have to check for errors
                /// everywhere we use a regular expression since there is no point in
                /// trying to match anything with faulty regular expressions.
                try {
                    auto expr = Stream{value}.fold_ws();
                    SubstituteNoCapAndLiterals(expr);

                    /// Construct an environment regex if captures are used.
                    static constinit std::array<std::string_view, 3> delims{"?<"sv, R"(\k<)", "$"sv};
                    if (Stream{value}.skip_to_any(delims).size() >= 2) {
                        Add(EnvironmentRegex{expr, state.literal_chars, state.pragmas["captype"]}, d);
                    } else {
                        Add(Regex{expr}, d);
                    }
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
}

auto Context::CreatePrefixState(std::string prefix) -> PrefixState* {
    return &states_by_prefix.emplace_back(
        std::move(prefix),
        std::vector<Check>{},
        default_pragmas,
        default_literal_chars
    );
}

int Context::Run() {
    /// If we don’t know what the prefix is, look for a
    /// prefix directive.
    Stream chfile{check_file.contents};
    const bool have_command_line_prefix = not states_by_prefix[0].prefix.empty();
    if (not have_command_line_prefix) {
        states_by_prefix[0].prefix = Trim(
            Stream{chfile}
                .skip_to(DirectiveNames[+Directive::Prefix])
                .skip_to_ws()
                .skip_ws()
                .read_to("\n")
        );

        if (states_by_prefix[0].prefix.empty()) Diag::Fatal(
            "No prefix provided and no {} directive found in check file",
            DirectiveNames[+Directive::Prefix]
        );
    }

    /// Collect check directives.
    CollectDirectives(states_by_prefix[0]);

    /// Don’t even bother matching if there was an error.
    if (has_error) return 1;

    /// Can’t check anything w/ no directives.
    if (run_directives.empty()) Diag::Fatal(
        "No {} directives found in check file!",
        DirectiveNames[+Directive::Run]
    );

    /// Dew it.
    for (auto& [cmd, state] : run_directives) RunTest(cmd, *state);
    return has_error ? 1 : 0;
}

void Context::RunTest(std::string_view test, PrefixState& state) {
    /// Substitute occurrences of `%s` with the file name.
    auto cmd = std::string{test};
    for (auto& [n, v] : definitions) utils::ReplaceAll(cmd, n, v);
    utils::ReplaceAll(cmd, "%s", check_file.path.string());

    /// Warn about unknown '%' defines.
    if (auto pos = cmd.find('%'); pos != std::string::npos) {
        auto def = Stream{std::string_view(cmd.data() + pos, cmd.size() - pos)}.read_to_ws();

        /// Find location of directive in original string.
        std::string_view def_str = test;
        if (auto def_pos = def_str.find(def); def_pos != std::string_view::npos)
            def_str = def_str.substr(def_pos, def.size());

        /// Error because the command is likely nonsense if we don’t.
        Diag::Error(
            this,
            LocationIn(def_str, check_file),
            "'{}' is not defined. Define it on the command-line using '-D {}=...'",
            def,
            def.substr(1)
        );

        /// Don’t even bother running this.
        return;
    }

    /// Run the command and get its output.
    if (verbose) fmt::print(stderr, "[FCHK] Running command: {}\n", cmd);
    File input_file{GetProcessOutput(cmd), "<input>"};
    detail::Matcher::Match(*this, input_file, state.checks);
}
