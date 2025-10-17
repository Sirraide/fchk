#include <base/FS.hh>
#include <base/Text.hh>
#include <clopts.hh>
#include <cmath>
#include <core.hh>
#include <errs.hh>
#include <unordered_map>

#ifdef _WIN32
#    define NOMINMAX
#    include <Windows.h>
#endif

namespace fchk::detail {
using namespace command_line_options;
using options = clopts< // clang-format off
    option<"--prefix", "Check prefix to use">,
    multiple<option<"-l", "Treat character(s) as literal">>,
    multiple<option<"-P", "Set a pragma">>,
    multiple<option<"-D", "Define a constant that can be used in 'R' directives">>,
    option<"--colours", "Enable colours in diagnostics", values<"auto", "always", "never">>,
    flag<"--stdout", "Print to stdout instead of stderr">,
    flag<"-a", "Abort on the first failed check">,
    flag<"-v", "Show more verbose error messages">,
    flag<"--nobuiltin", "Disable builtin magic variables (e.g. $LINE)">,
    flag<"--update", "Update tests instead of checking them">,
    positional<"checkfile", "File containing the check directives", file<>, true>,
    help<>
>; // clang-format on
} // namespace fchk::detail

using namespace fchk;

namespace {
/// Check if this is a known builtin.
constexpr bool IsBuiltin(std::string_view name) {
    return name == "LINE";
}

struct ExecutionResult {
    std::string output;
    std::string error_message;
    bool success = false;
};

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

    if (sz == 0) return std::format("Unknown error: {}", err);
    std::string msg{buffer, sz};
    LocalFree(buffer);
    return msg;
}
#endif

auto RunCommand(
    [[maybe_unused]] Context& C,
    [[maybe_unused]] Location cmd_loc,
    std::string_view cmd
) -> ExecutionResult {
    ExecutionResult er;

#ifndef _WIN32
    auto pipe = popen(cmd.data(), "r");
    if (not pipe) {
        er.error_message = std::format("Failed to run command '{}': {}", cmd, std::strerror(errno));
        return er;
    }

    static constexpr usz bufsize = 4'096;
    for (;;) {
        er.output.resize(er.output.size() + bufsize);
        auto read = std::fread(er.output.data() + er.output.size() - bufsize, 1, bufsize, pipe);
        if (read < bufsize) er.output.resize(er.output.size() - (bufsize - read));
        if (std::ferror(pipe)) {
            er.error_message = std::format("Error reading file: {}", std::strerror(errno));
            return er;
        }
        if (std::feof(pipe)) break;
    }
    auto code = pclose(pipe);
    if (not WIFEXITED(code)) {
        er.error_message = std::format("Command '{}' exited abnormally", cmd);
        return er;
    }

    if (WEXITSTATUS(code) != 0) {
        er.error_message = std::format("Command '{}' exited with status {}", cmd, WEXITSTATUS(code));
        return er;
    }

    er.success = true;
    return er;

#else
    HANDLE pipe_read{}, pipe_write{};
    SECURITY_ATTRIBUTES sa{};
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = nullptr;

    /// Create the pipe
    if (not CreatePipe(&pipe_read, &pipe_write, &sa, 0)) {
        er.error_message = std::format("Failed to create pipe: {}", GetWindowsError());
        return er;
    }

    /// Create the child process.
    STARTUPINFO si{};
    PROCESS_INFORMATION pi{};
    si.cb = sizeof(STARTUPINFO);
    si.hStdError = pipe_write;
    si.hStdOutput = pipe_write;
    si.dwFlags |= STARTF_USESTDHANDLES;

    auto command = std::format("cmd /c {}", cmd);
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
        er.error_message = std::format("Failed to create process: {}", GetWindowsError());
        return er;
    }

    /// Close the write end of the pipe.
    CloseHandle(pipe_write);

    /// Read the output.
    for (;;) {
        static constexpr usz bufsize = 4'096;
        er.output.resize(er.output.size() + bufsize);
        DWORD read{};
        if (not ReadFile(pipe_read, er.output.data() + er.output.size() - bufsize, bufsize, &read, nullptr)) {
            if (GetLastError() == ERROR_BROKEN_PIPE) break;
            er.error_message = std::format("Failed to read from pipe: {}", GetWindowsError());
            return er;
        }
        if (read == 0) break;
        er.output.resize(er.output.size() - (bufsize - read));
    }

    /// Close the read end of the pipe.
    CloseHandle(pipe_read);

    /// Wait for the process to exit.
    WaitForSingleObject(pi.hProcess, INFINITE);

    /// Check its exit code.
    DWORD exit_code{};
    if (GetExitCodeProcess(pi.hProcess, &exit_code) == FALSE) {
        er.error_message = std::format("Failed to get exit code: {}", GetWindowsError());
        return er;
    }

    if (exit_code != 0) {
        er.error_message = std::format("Command '{}' exited with status {}", cmd, exit_code);
        return er;
    }

    /// Close the process and thread handles.
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    /// Done!
    er.success = true;
    return er;
#endif
}
} // namespace

Context::Context(
    std::shared_ptr<DiagsHandler> dh,
    std::string check,
    fs::Path check_name,
    std::string prefix,
    StringMap<bool> pragmas,
    std::unordered_set<char> literal_chars,
    std::span<const std::string> defines,
    bool abort_on_error,
    bool verbose,
    bool enable_builtins,
    bool update
) : dh{std::move(dh)},
    check_file{std::move(check), std::move(check_name)},
    default_pragmas(std::move(pragmas)),
    default_literal_chars(std::move(literal_chars)),
    abort_on_error(abort_on_error),
    verbose(verbose),
    enable_builtins(enable_builtins),
    update(update) {
    /// Initialise known pragmas.
    if (not default_pragmas.contains("re")) default_pragmas["re"] = false;
    if (not default_pragmas.contains("nocap")) default_pragmas["nocap"] = false;
    if (not default_pragmas.contains("captype")) default_pragmas["captype"] = false;

    /// Create initial state.
    CreatePrefixState(prefix);

    /// Save definitions.
    for (str d : defines) {
        auto name = d.take_until('=');
        if (not d.consume('=')) Error(Location(), ERR_DRV_D_OPT_INVALID);
        if (name == "s" or name == "t") Error(Location(), ERR_DRV_ST_REDEF, name);
        if (name.contains_any(str::whitespace())) Error(Location(), "Defined names may not contain whitespace");
        definitions.add(std::format("%{}", name), d.text());
    }

    /// The '%t' variable is global per file.
    definitions.add("%t", fs::TempPath());
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

bool Regex::match(std::string_view str) const noexcept {
    return re.match(str);
}

EnvironmentRegex::EnvironmentRegex(
    std::string pattern,
    std::unordered_set<char> literal_chars,
    bool captype,
    bool builtins_enabled
) : re_str(std::move(pattern)),
    literal_chars(std::move(literal_chars)) {
    /// Preprocessing step for typed captures.
    if (captype) {
        std::string processed;
        str s{re_str};
        for (;;) {
            processed += s.take_until("$");
            auto saved = s;
            s.drop();
            if (s.empty()) {
                processed += saved.text();
                break;
            }

            /// Find ":".
            auto capture = s.take_while([](char c) { return std::isalnum(u8(c)) or c == '_'; });

            /// Got one. Ignore if this is a builtin.
            if (
                (not builtins_enabled or not IsBuiltin(capture)) and
                s.starts_with(':') and
                (s.drop(), not s.empty() and not s.starts_with_any(" \t\n\r\f\v"))
            ) {
                auto type = s.take_while([](char c) { return std::isalnum(u8(c)) or c == '_'; });
                processed += std::format("(?<{}>${})", capture, type);
            }

            /// Regular capture.
            else {
                processed += saved.take(saved.size() - s.size());
                s = saved;
            }
        }
        re_str = std::move(processed);
    }

    /// Find all captures defined by this pattern.
    str s{re_str};
    for (;;) {
        auto group_name = s.drop_until("?<").drop(2).take_until_or_empty(">");
        if (s.empty()) break;
        if (IsBuiltin(group_name)) continue;
        defined_captures.emplace(group_name);
    }
}

/// Perform variable substitution.
///
/// Replace occurrences of variables with their respective values; if
/// \p re is not null, escape any variables defined by that regex
/// so we can capture them while matching. If \p re is null, then this
/// cannot handle variable definitions.
///
/// \param C The fchk context.
/// \param loc The location to use for error messages.
/// \param env The environment to use for variable substitution.
/// \param input The input string to substitute variables in.
/// \param [opt] re Associated regex that defines captures.
/// \throw Regex::Exception If there is an error during substitution.
/// \return The substituted string.
auto SubstituteVars(
    Context& C,
    Location loc,
    const Environment& env,
    std::string_view input,
    EnvironmentRegex* re = nullptr
) -> std::string {
    std::string subst;

    /// Substitute named captures that are not defined by this RE and
    /// convert dollar-style captures that are to PCRE2-style '\k<name>'
    /// captures.
    for (str s{input};;) {
        static constinit std::array<std::string_view, 2> delims{R"(\k<)", "$"sv};
        static const auto IsCaptureGroupName = [](char c) { return std::isalnum(u8(c)) or c == '_'; };
        const auto Add = [&](std::string_view capture, bool escape) {
            if (C.BuiltinsEnabled() and IsBuiltin(capture)) {
                if (capture == "LINE") {
                    if (not loc.seekable()) throw Regex::Exception(
                        "Cannot evaluate $LINE variable due to invalid source location"
                    );

                    /// Directive may be followed by a '+' or '-' and a number.
                    auto lc = loc.seek_line_column();
                    if (s.starts_with_any("+-")) {
                        bool negative = s.starts_with("-");
                        auto num = s.drop().take_while([](char c) { return '0' <= c and c <= '9'; });
                        isz i{};
                        auto res = std::from_chars(num.begin(), num.end(), i);
                        if (res.ec != std::error_code{}) throw Regex::Exception(
                            "Failed to parse $LINE offset '{}': {}",
                            num,
                            make_error_code(res.ec).message()
                        );

                        if (negative) i = -i;
                        if (i + isz(lc.line) < 1) throw Regex::Exception(
                            "Line number overflow. Cannot add {} to current line number {}",
                            i,
                            lc.line
                        );

                        lc.line = usz(isz(lc.line) + i);
                    }

                    subst += std::to_string(lc.line);
                } else {
                    Unreachable("Unknown builtin");
                }
            } else if (auto var = rgs::find(env, capture, &EnvEntry::name); var != env.end()) {
                if (not escape and not var->literal) {
                    /// Handle nested expansions.
                    if (var->value.contains('$') or var->value.contains("\\k<")) {
                        /// Nested expansions may not reference captures defined by this RE.
                        subst += SubstituteVars(C, loc, env, var->value, nullptr);
                    } else {
                        subst += var->value;
                    }
                }

                /// Always escape literal variables.
                else {
                    subst += std::format("\\Q{}\\E", var->value);
                }
            } else {
                throw Regex::Exception("Undefined capture '{}'", capture);
            }
        };

        /// Get start of next capture group.
        auto fragment = s.take_until_any(delims);
        subst += fragment;

        /// An '$' on its own is not a capture group, so we always need
        /// at least two characters (the '$' and another character) for
        /// this to be a capture.
        if (s.size() < 2) break;

        /// PCRE2-style '\k<name>' capture group.
        if (s.front() == '\\') {
            s.drop(R"(\k<)"sv.size());
            auto name = s.take_while(IsCaptureGroupName);
            if (re and re->defined_captures.contains(name)) subst += std::format("\\k<{}>", name);
            else Add(name, false);
            s.drop(">"sv.size());
        }

        /// Dollar capture, w/ optional escaping.
        else {
            bool escape = false;
            s.drop("$"sv.size());
            if (s.starts_with("$")) {
                s.drop("$"sv.size());
                escape = true;
            }

            auto name = s.take_while(IsCaptureGroupName);
            if (re and re->defined_captures.contains(name)) subst += std::format("\\k<{}>", name);
            else Add(name, escape);
        }
    }

    return subst;
}

auto EnvironmentRegex::substitute_vars(Context& ctx, Location loc, const Environment& env) -> std::string {
    return SubstituteVars(ctx, loc, env, re_str, this);
}

/// ===========================================================================
///  Diagnostics
/// ===========================================================================
namespace {
/// Get the name of a diagnostic.
constexpr std::string_view Name(DiagsHandler::Kind kind) {
    using Kind = DiagsHandler::Kind;
    switch (kind) {
        case Kind::Error: return "Error";
        case Kind::Warning: return "Warning";
        case Kind::Note: return "Note";
        default: return "Diagnostic";
    }
}
} // namespace

void DiagsHandler::report_impl(Context* ctx, Kind kind, Location where, std::string_view msg, bool no_line) {
    /// Separate error messages w/ an empty line.
    if (ctx) {
        if (kind != Kind::Note and ctx->has_diag) print("\n");
        ctx->has_diag = true;
    }

    /// If the diagnostic is an error, set the error flag.
    if (kind == Kind::Error and ctx)
        ctx->has_error = true; /// Separate line so we can put a breakpoint here.

    /// If the location is invalid, either because the specified file does not
    /// exists, its position is out of bounds or 0, or its length is 0, then we
    /// skip printing the location.
    if (not where.seekable()) {
        /// Even if the location is invalid, print the file name if we can.
        if (where.file) print("{}{}: ", colour(Default), where.file->path.string());

        /// Print the message.
        print("{}{}: ", colour(kind), Name(kind));
        print("{}{}\n", colour(Default), msg);
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
    before = str(before).replace("\t", "    ");
    range = str(range).replace("\t", "    ");
    after = str(after).replace("\t", "    ");

    /// Print the file name, line number, and column number.
    print("{}{}:{}:{}: ", colour(Default), where.file->path.string(), line, col + 1);

    /// Print the diagnostic name and message.
    print("{}{}: ", colour(kind), Name(kind));
    print("{}{}\n", msg, colour(Reset));

    /// Print the line up to the start of the location, the range in the right
    /// colour, and the rest of the line.
    if (no_line) return;
    print(" {} │ {}", line, before);
    print("{}{}{}", colour(kind), range, colour(Reset));
    print("{}\n", after);

    /// Determine the number of digits in the line number.
    const auto digits = std::to_string(line).size();

    /// Determine the column width of the text.
    static const auto ColumnWidth = [](std::string_view text) {
        usz wd = 0;
        for (auto c : text) {
            if (std::iscntrl(c)) continue;
            if (c == '\t') wd += 4;
            else wd++;
        }
        return wd;
    };

    /// Underline the range. For that, we first pad the line based on the number
    /// of digits in the line number and append more spaces to line us up with
    /// the range.
    for (usz i = 0, end = digits + ColumnWidth(before) + sizeof("  | ") - 1; i < end; i++)
        print(" ");

    /// Finally, underline the range.
    write(colour(kind));
    for (usz i = 0, end = ColumnWidth(range); i < end; i++) write("~");
    print("{}\n", colour(Reset));
}

void DiagsHandler::write(std::string_view text) {
    std::print(stream, "{}", text);
}

/// ===========================================================================
///  Implementation
/// ===========================================================================
namespace fchk::detail {
class Matcher {
    struct Line {
        std::string text;
        Location loc;
    };

    Context* const C;
    [[maybe_unused]] CheckFile& input_file;
    std::span<Check> checks;
    std::vector<Line> input_lines;
    std::vector<Line>::iterator in, prev;
    std::span<Check>::iterator chk;
    Environment env;

    /// Whether we’re in a local environment.
    bool in_local_env = false;

    /// For providing context around a line in error messages.
    struct LineContext {
        Matcher& M;
        decltype(in) it = M.in, prev = M.prev;

        void print(std::string_view msg) const {
            /// Print the message w/o the code line.
            M.C->Note(it->loc, "{}", msg);

            /// Print only a couple of lines so we don’t dump 2000 lines of output
            /// if the input is long. We start printing one line before the one we
            /// started matching from;
            const auto lc = it->loc.seek_line_column();
            const auto start = lc.line == 1 ? 1 : lc.line - 1;
            for (auto [i, line] : vws::enumerate(rgs::subrange{prev, M.input_lines.end()})) {
                static constexpr usz max_lines = 7;
                if (usz(i) >= max_lines) break;
                M.C->dh->print(" {: >{}} │ ", start + usz(i), std::to_string(start + max_lines - 1).size());
                if (i == 1 - (lc.line == 1)) {
                    M.C->dh->print(
                        "{}{}\n{}",
                        M.C->dh->colour(DiagsHandler::Kind::Note),
                        line.text.empty() ? "<empty>" : line.text,
                        M.C->dh->colour(DiagsHandler::Colour::Reset)
                    );
                } else {
                    M.C->dh->print("{}\n", line.text);
                }
            }
        }

        void restore() {
            M.in = it;
            M.prev = prev;
        }
    };

    Matcher(Context& ctx, CheckFile& input_file, std::span<Check> checks) : C{&ctx}, input_file{input_file}, checks{checks} {
        const auto ProcessLine = [&](auto&& r) -> Line {
            auto sv = std::string_view{&*r.begin(), usz(rgs::distance(r))};
            return {str{sv}.fold_ws(), ctx.LocationIn(sv, input_file)};
        };

        /// Split input into lines.
        auto range = input_file.contents | vws::split('\n') | vws::transform(ProcessLine);
        input_lines = {range.begin(), range.end()};
        prev = in = input_lines.begin();
        chk = this->checks.begin();
    }

    /// Match a regular expression that uses the environment.
    bool MatchEnvRegex(Location check_location, EnvironmentRegex& re) {
        /// Ensure defined captures don’t overwrite the ENV.
        for (auto& c : re.defined_captures)
            if (rgs::contains(env, c, &EnvEntry::name))
                throw RedefError(c);

        /// Compile the RE and execute it.
        regex expr{re.substitute_vars(*C, check_location, env)};
        auto res = expr.match(in->text);
        if (not res) return false;

        /// If the RE matches, extract the captures.
        for (const auto& name : re.defined_captures) {
            auto cap = expr[name];
            if (not cap) throw Regex::Exception("Failed to get capture index for '{}'", name);
            Define(name, cap->extract(std::string_view{in->text}), true);
        }

        return true;
    }

    /// Advance the line and save the previous one.
    void NextLine() {
        prev = in;
        ++in;
    }

    /// Match a line.
    bool MatchLine() {
        if (auto s = std::get_if<std::string>(&chk->data)) return in->text.contains(*s);
        if (auto re = std::get_if<Regex>(&chk->data)) return re->match(in->text);
        return MatchEnvRegex(chk->loc, std::get<EnvironmentRegex>(chk->data));
    }

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
        if (C->enable_builtins and IsBuiltin(key)) throw RedefError(std::string{key});
        if (rgs::contains(env, key, &EnvEntry::name)) throw RedefError(std::string{key});
        env.emplace_back(std::string{key}, std::string{value}, capture, in_local_env);
    }

    /// Issue an error at the current position and print the environment
    /// if the current directive is a regex directive and it is not empty.
    void PrintRegexError(std::string_view msg) {
        C->Error(chk->loc, "{}", msg);
        if (
            chk->dir == Directive::RegexCheckAny or
            chk->dir == Directive::RegexCheckNext or
            chk->dir == Directive::RegexCheckNotSame or
            chk->dir == Directive::RegexCheckNotAny or
            chk->dir == Directive::RegexCheckNotNext
        ) {
            if (env.empty()) return;
            if (C->verbose) {
                auto env_strs = env | vws::transform([](auto&& p) { return std::format("    {} = {}", p.name, p.value); });
                std::string joined;
                for (auto&& s : env_strs) joined += s + '\n';
                C->NoteNoLine(chk->loc, "With env: [\n{}\n]\n", joined);
            }

            /// Print expansion of regex that contains captures.
            if (auto re = std::get_if<EnvironmentRegex>(&chk->data)) {
                C->NoteNoLine(
                    chk->loc,
                    "Expands to: {}",
                    re->substitute_vars(*C, chk->loc, env)
                );

                /// Separate from the note after it.
                C->dh->print("\n");
            }
        }

        /// If this *isn’t* a regex error, but it contains something that looks
        /// like it could be a capture, ask the user if that’s what they meant.
        else if (
            auto s = std::get_if<std::string>(&chk->data);
            s and (s->contains("$") or s->contains("\\k<") or s->contains("(?<"))
        ) {
            auto dir = chk->dir == Directive::CheckNext    ? Directive::RegexCheckNext
                     : chk->dir == Directive::CheckAny     ? Directive::RegexCheckAny
                     : chk->dir == Directive::CheckNotSame ? Directive::RegexCheckNotSame
                     : chk->dir == Directive::CheckNotNext ? Directive::RegexCheckNotNext
                                                           : Directive::RegexCheckNotAny;
            C->NoteNoLine(
                chk->loc,
                "Match contains regex captures, did you mean to use '{}' instead?",
                DirectiveNames[+dir]
            );
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
                    ++chk;
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
            case Directive::Verify:
            case Directive::XFail:
                Unreachable();

            case Directive::Begin: {
                /// Yeet everything defined after the last definition point.
                std::erase_if(env, &EnvEntry::local);
                in_local_env = true;
            } break;

            /// Use an env regex to expand the definition. An undefined variable
            /// here is an error.
            case Directive::ExpandDefine: {
                auto& line = std::get<std::string>(chk->data);
                str s{line};
                auto name = s.take_until_ws();
                auto value = s.trim().text();

                /// Expand vars.
                try {
                    Define(name, SubstituteVars(*C, chk->loc, env, value), false);
                } catch (const Regex::Exception& e) {
                    C->Error(chk->loc, "Could not expand '{}': {}", value, e.what());
                }
            } break;

            case Directive::Define: {
                auto& line = std::get<std::string>(chk->data);
                str s{line};
                auto name = s.take_until_ws();
                auto value = s.trim().text();
                Define(name, value, false);
            } break;

            case Directive::Undefine: {
                auto& var = std::get<std::string>(chk->data);

                /// Handle builtins.
                if (C->enable_builtins and IsBuiltin(var)) C->Warning(
                    chk->loc,
                    "Builtin variables cannot be undefined. "
                    "Pass --nobuiltin to disable them"
                );

                else if (var == "*") env.clear();
                else if (auto it = rgs::find(env, var, &EnvEntry::name); it != env.end()) env.erase(it);
                else C->Warning(chk->loc, "Variable '{}' is not defined", var);
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
                    C->Error(chk->loc, "Input contains prohibited string");
                    context.print("In this line");
                }

                AdvanceLineAfterCheck();
            } break;

            /// Check that this line does not match a regular expression.
            case Directive::RegexCheckNotSame: {
                if (auto re = std::get_if<Regex>(&chk->data)) {
                    if (re->match(in->text)) {
                        C->Error(chk->loc, "Input contains prohibited string");
                        context.print("In this line");
                    }
                }

                else if (auto env_re = std::get_if<EnvironmentRegex>(&chk->data)) {
                    if (MatchEnvRegex(chk->loc, *env_re)) {
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
                C->Error(chk->loc, "Invalid regular expression: {}", e.message);
            } catch (const RedefError& e) {
                if (C->enable_builtins and IsBuiltin(e.var)) {
                    C->Error(
                        chk->loc,
                        "Cannot redefine builtin variable '{}'. Pass --nobuiltin to disable builtin variables",
                        e.var
                    );
                } else {
                    C->Error(
                        chk->loc,
                        "'{}' is already defined. Use 'u {}' to undefine it.",
                        e.var,
                        e.var
                    );
                }
            }

            /// Halt on error if requested.
            if (C->has_error and C->abort_on_error) return;

            /// Take care not to go out of bounds here.
            if (chk == checks.end()) return;
        }

        /// If we have more checks than input lines, we have a problem,
        /// except if the checks are negative checks, which we can simply
        /// discard.
        while (chk != checks.end() and chk->is_negative_check()) ++chk;
        if (chk != checks.end() and not C->has_error) C->Error(
            chk->loc,
            "End of file reached looking for string"
        );
    }

public:
    static void Match(Context& ctx, CheckFile& input_file, std::span<Check> checks) {
        Matcher{ctx, input_file, checks}.Match();
    }
};

static_assert(DirectiveNames.size() == 19, "Update the map below when directives are added");
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
    {DirectiveNames[+Directive::ExpandDefine], Directive::ExpandDefine},
    {DirectiveNames[+Directive::Undefine], Directive::Undefine},
    {DirectiveNames[+Directive::Pragma], Directive::Pragma},
    {DirectiveNames[+Directive::Prefix], Directive::Prefix},
    {DirectiveNames[+Directive::Run], Directive::Run},
    {DirectiveNames[+Directive::Verify], Directive::Verify},
    {DirectiveNames[+Directive::XFail], Directive::XFail},
};

const auto RunWithPrefixDirectiveNameStart = std::format("{}[", DirectiveNames[+Directive::Run]);

} // namespace fchk::detail

void Context::CollectDirectives(PrefixState& state) {
    stream chfile{check_file.contents};

    /// Check if a directive causes a new test to be run.
    auto IsRunDirective = [](Directive dir) {
        switch (dir) {
            default: return false;
            case Directive::Run:
            case Directive::Verify:
            case Directive::XFail:
                return true;
        }
    };

    /// Read a directive’s argument, if any.
    auto ReadArgument = [&] -> std::string_view {
        return chfile.take_until("\n").trim().text();
    };

    /// Find all directives in the file.
    while (not chfile.empty()) {
        /// Only a line that starts with the prefix followed by whitespace is a directive.
        chfile.drop_until(state.prefix).drop(state.prefix.size());
        if (not chfile.starts_with_any(str::whitespace())) {
            chfile.drop_until('\n').drop();
            continue;
        }

        /// Delete the whitespace before the directive. If we end up eating a newline too,
        /// then this line contains only whitespace.
        if (chfile.take_while_any(str::whitespace()).contains('\n'))
            continue;

        /// Handle run directives.
        if (chfile.consume(detail::RunWithPrefixDirectiveNameStart)) {
            auto prefix = chfile.take_until_any("]\r\n").trim().text();
            if (not chfile.starts_with("]")) {
                chfile.drop_until_any("\r\n");
                continue;
            }

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

            /// Drop the ']'.
            chfile.drop();

            /// Add a run directive.
            run_directives.emplace_back(ReadArgument(), new_state);
            continue;
        }

        /// Read directive.
        auto dir = chfile.take_until_any(" \t\v\f\r\n").trim().text();
        if (dir.empty()) continue;

        /// Check if this really is a directive.
        auto it = detail::NameDirectiveMap.find(dir);
        if (it == detail::NameDirectiveMap.end()) continue;
        auto value = ReadArgument();

        /// Handle spurious prefix directives
        if (it->second == Directive::Prefix) {
            /// Overriding the prefix is not allowed anymore.
            if (state.prefix != value) Warning(
                LocationIn(dir, check_file),
                "Conflicting prefix directive '{}' ignored (current prefix is '{}')",
                value,
                state.prefix
            );
            continue;
        }

        /// Run/Verify directives.
        if (IsRunDirective(it->second)) {
            run_directives.emplace_back(
                value,
                &state,
                it->second != Directive::Run,
                it->second == Directive::XFail
            );
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

            /// Escape characters to be treated literally.
            expr = str(expr).escape(std::string(state.literal_chars.begin(), state.literal_chars.end()));
        };

        /// Handle directive.
        switch (it->second) {
            case Directive::Prefix:
            case Directive::Run:
            case Directive::Verify:
            case Directive::XFail:
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

                Add(str{value}.fold_ws(), d);
                break;

            case Directive::Undefine:
                Add(str{value}.fold_ws(), d);
                break;

            case Directive::Begin:
                Add("", d);
                break;

            /// Value of a define must honour literals at definition time. Variables
            /// for expanded definitions will be substituted at match time instead.
            case Directive::Define:
            case Directive::ExpandDefine: {
                auto text = str{value}.fold_ws();
                SubstituteNoCapAndLiterals(text);
                Add(text, d);
            } break;

            /// Handle pragmas.
            case Directive::Pragma: {
                if (value.empty()) {
                    Error(LocationIn(value, check_file), "'p' directive requires an argument");
                    continue;
                }

                auto s = str{value};
                auto name = s.take_until_ws();
                auto arg = s.trim_front().take_until_ws();

                /// Some pragmas require special handling.
                if (name == "lit" or name == "nolit") {
                    /// No-op if no chars were provided
                    if (arg.empty()) {
                        Warning(
                            LocationIn(arg.text(), check_file),
                            "Empty '{}' pragma ignored",
                            name
                        );

                        continue;
                    }

                    /// Making backslashes literal would break things.
                    if (name == "lit" and arg.contains('\\')) Error(
                        LocationIn(arg.text(), check_file),
                        "Escape character '\\' cannot be made literal",
                        name
                    );

                    /// Warn about '(' and ')'.
                    if (arg.contains('(') or arg.contains(')')) Warning(
                        LocationIn(arg.text(), check_file),
                        "Prefer using 'p nocap' over making '(' or ')' (not) literal "
                        "as the latter can cause the regex engine to error."
                    );

                    /// Tell the user that `off` isn’t supported for this pragma. Since
                    /// 'o' and 'f' are not metacharacters anyway, passing 'off' to this
                    /// wouldn’t to anything even if we accepted it.
                    if (arg == "off") {
                        Warning(
                            LocationIn(arg.text(), check_file),
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
                        Warning(LocationIn(name.text(), check_file), "Unknown pragma ignored");

                        continue;
                    }

                    /// Pragmas take an optional ‘off’ parameter.
                    if (not arg.empty() and arg != "off") Warning(
                        LocationIn(arg.text(), check_file),
                        "Unknown pragma argument ignored"
                    );

                    /// Set the pragma.
                    state.pragmas[std::string{name}] = arg != "off";
                }

                /// Warn about junk.
                if (not s.trim_front().empty()) Warning(
                    LocationIn(s.text(), check_file),
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
                    auto expr = str{value}.fold_ws();
                    SubstituteNoCapAndLiterals(expr);

                    /// Construct an environment regex if captures are used.
                    static constinit std::array<std::string_view, 3> delims{"?<"sv, R"(\k<)", "$"sv};
                    if (str{value}.drop_until_any(delims).size() >= 2) {
                        Add(EnvironmentRegex{std::move(expr), state.literal_chars, state.pragmas["captype"], enable_builtins}, d);
                    } else {
                        Add(Regex{std::move(expr)}, d);
                    }
                } catch (const Regex::Exception& e) {
                    Error(
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
    str chfile{check_file.contents};
    const bool have_command_line_prefix = not states_by_prefix[0].prefix.empty();
    if (not have_command_line_prefix) {
        states_by_prefix[0].prefix = auto{chfile}
            .drop_until(DirectiveNames[+Directive::Prefix])
            .drop_until_ws()
            .trim_front()
            .take_until("\n")
            .trim()
            .text();

        if (states_by_prefix[0].prefix.empty()) {
            Error(
                Location(),
                "No prefix provided and no {} directive found in check file",
                DirectiveNames[+Directive::Prefix]
            );

            // We can’t do anything if there is no prefix.
            return 1;
        }
    }

    /// Collect check directives.
    CollectDirectives(states_by_prefix[0]);

    /// Can’t check anything w/ no directives.
    if (run_directives.empty()) Error(
        Location(),
        "No {} directives found in check file!",
        DirectiveNames[+Directive::Run]
    );

    /// Don’t even bother matching if there was an error.
    if (has_error) return 1;

    /// Dew it.
    for (auto& rd : run_directives) {
        RunTest(rd);
        if (has_error and abort_on_error) break;
    }

    /// Update the test file if that was requested.
    if (update) {
        /// If the output file doesn’t exist, just print the output to
        /// the diagnostics engine. This is used e.g. for tests.
        if (not File::Exists(check_file.path)) {
            dh->write(updated_output);
            return 0;
        }

        /// If it does, we need to delete any tests we’ve previously put
        /// there, if any. Skip over any lines at the end of the file that
        /// start with any of the prefixes we know of.
        auto in_lines = str{check_file.contents}.lines() | rgs::to<std::vector>();
        auto rev = in_lines.rbegin();
        auto end = in_lines.rend();
        for (; rev != end; ++rev) {
            str l = *rev;
            auto StartsWithPrefix = [&] {
                for (const auto& p : states_by_prefix)
                    if (l.starts_with(p.prefix))
                        return true;
                return false;
            };
            if (not l.empty() and not StartsWithPrefix()) break;
        }

        // Skip over any empty lines.
        for (; rev != end; ++rev) {
            str l = *rev;
            if (not l.empty()) break;
        }

        // Append any remaining lines.
        std::string combined;
        for (auto l : rgs::subrange{rev, end} | vws::reverse) {
            combined += l.text();
            combined += '\n';
        }

        // Append the new lines.
        combined += '\n';
        combined += updated_output;

        // Finally, overwrite the file.
        if (auto e = File::Write(check_file.path, combined); not e)
            Error(Location(), "Failed to update output file: {}", e.error());
    }

    return has_error ? 1 : 0;
}

int Context::RunMain(std::shared_ptr<DiagsHandler> dh, int argc, char** argv) {
    auto opts = detail::options::parse(argc, argv, dh->get_error_handler());

    /// User-provided prefix may not be empty.
    if (auto pre = opts.get<"--prefix">(); pre and str(*pre).trim().empty()) {
        dh->report(
            nullptr,
            DiagsHandler::Kind::Error,
            Location(),
            ERR_DRV_PREFIX_OPT_INVALID
        );
        return 1;
    }

    /// Collect pragmas.
    StringMap<bool> pragmas;
    for (auto v : opts.get<"-P">()) pragmas[v] = true;

    /// Collect literal chars.
    std::unordered_set<char> literal_chars;
    for (auto v : opts.get<"-l">())
        for (auto c : v)
            literal_chars.insert(c);

    if (opts.get<"-v">()) dh->report(
        nullptr,
        DiagsHandler::Kind::Note,
        Location(),
        std::format("[FCHK] Running fchk version {}\n", FCHK_VERSION)
    );

    /// Check if we should use colours.
    auto colours_opt = opts.get<"--colours">("auto");
    dh->stream = opts.get<"--stdout">() ? stdout : stderr;
    dh->enable_colours = colours_opt == "auto"
                           ? isatty(fileno(dh->stream))
                           : colours_opt == "always";

    Context ctx{
        dh,
        std::move(opts.get<"checkfile">()->contents),
        std::move(opts.get<"checkfile">()->path),
        opts.get<"--prefix">(""),
        std::move(pragmas),
        std::move(literal_chars),
        opts.get<"-D">(),
        opts.get<"-a">(),
        opts.get<"-v">(),
        not opts.get<"--nobuiltin">(),
        opts.get<"--update">(),
    };

    return ctx.Run();
}

void Context::RunTest(Test& test) {
    /// Update builtin definitions.
    definitions.add("%s", check_file.path.string());

    /// Warn about unknown '%' defines. Do this BEFORE substitution since it’s
    /// valid for the replacement text to contain '%'.
    for (str s{test.run_directive}; not s.drop_until('%').empty();) {
        auto def = s.take_until_ws();
        if (definitions.is_prefix_of(def)) continue;

        /// If this is wrapped in quotes, drop them from the diagnostic.
        def.drop_back_while_any("\'\"");

        /// Error because the command is likely nonsense if we don’t.
        Error(
            LocationIn(def.text(), check_file),
            "'{}' is not defined. Define it on the command-line using '-D {}=...'",
            def,
            def.drop()
        );

        /// Don’t even bother running this.
        return;
    }

    /// Run the command and get its output.
    auto cmd = definitions.replace(test.run_directive);
    VerboseLog("[FCHK] Running command: {}\n", cmd);
    auto res = RunCommand(*this, LocationIn(test.run_directive, check_file), cmd);

    /// In update mode, just collect the output prefixed by the prefix; ignore
    /// verify only commands here.
    if (update and not test.verify_only) {
        if (not updated_output.empty()) updated_output += '\n';
        bool first = true;
        for (auto l : str{res.output}.trim().lines()) {
            auto t = l.trim_back().text();
            updated_output += std::format(
                "{} {}{}{}\n",
                test.state->prefix,
                first ? '*' : '+',
                t.empty() ? ""sv : " "sv,
                t
            );
            first = false;
        }
        return;
    }

    /// Return early if the command failed.
    if (not res.success) {
        /// This was expected.
        if (test.xfail) return;

        /// Don’t print an error in verify mode.
        if (test.verify_only) {
            dh->write(res.output);
        } else {
            Error(
                LocationIn(test.run_directive, check_file),
                "Command '{}' failed: {}\n{}",
                cmd,
                res.error_message,
                res.output
            );
        }

        has_error = true;
        return;
    }

    /// We were supposed to fail.
    if (test.xfail) {
        Error(
            LocationIn(test.run_directive, check_file),
            "Command '{}' succeeded even though it was expected to fail",
            cmd
        );

        has_error = true;
        return;
    }

    /// In verify mode, there is nothing else to do.
    if (test.verify_only) return;
    CheckFile input_file{res.output, "<input>"};
    detail::Matcher::Match(*this, input_file, test.state->checks);
}
