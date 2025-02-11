#ifndef FCHK_CORE_HH
#define FCHK_CORE_HH

#include <base/Base.hh>
#include <base/FS.hh>
#include <clopts.hh>
#include <deque>
#include <functional>
#include <map>
#include <print>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

namespace fchk {
using namespace base;

class Context;

/// A decoded source location.
struct LocInfo {
    usz line;
    usz col;
    const char* line_start;
    const char* line_end;
};

/// A short decoded source location.
struct LocInfoShort {
    usz line;
    usz col;
};

/// A file in the context.
struct File {
    std::string contents;
    fs::Path path;
};

/// A source range in a file.
struct Location {
    u32 pos{};
    u32 len{};
    File* file{};

    constexpr Location() = default;
    constexpr Location(u32 pos, u16 len, File* file)
        : pos(pos), len(len), file(file) {}

    /// Create a new location that spans two locations.
    constexpr Location(Location a, Location b) {
        if (a.file != b.file) return;
        if (not a.is_valid() or not b.is_valid()) return;
        pos = std::min<u32>(a.pos, b.pos);
        len = u16(std::max<u32>(a.pos + a.len, b.pos + b.len) - pos);
    }

    [[nodiscard]] constexpr bool is_valid() const { return len != 0; }

    /// Seek to a source location.
    [[nodiscard]] auto seek() const -> LocInfo;

    /// Seek to a source location, but only return the line and column.
    [[nodiscard]] auto seek_line_column() const -> LocInfoShort;

    /// Check if the source location is seekable.
    [[nodiscard]] bool seekable() const;
};

/// Directive prefixes. Do NOT reorder these without
/// also updating the DirectiveToRegexDirective and
/// DirectiveNames arrays below.
enum struct Directive : u8 {
    CheckAny,
    CheckNext,
    CheckNotSame,
    CheckNotAny,
    CheckNotNext,
    RegexCheckAny,
    RegexCheckNext,
    RegexCheckNotSame,
    RegexCheckNotAny,
    RegexCheckNotNext,
    Begin,
    Define,
    ExpandDefine,
    Undefine,
    Pragma,
    Prefix,
    Run,
    Verify,
    XFail,
    Count = XFail,
};

inline constexpr std::array DirectiveToRegexDirective{
    Directive::RegexCheckAny,
    Directive::RegexCheckNext,
    Directive::RegexCheckNotSame,
    Directive::RegexCheckNotAny,
    Directive::RegexCheckNotNext,
    Directive::RegexCheckAny,
    Directive::RegexCheckNext,
    Directive::RegexCheckNotSame,
    Directive::RegexCheckNotAny,
    Directive::RegexCheckNotNext,
    Directive::Begin,
    Directive::Define,
    Directive::ExpandDefine,
    Directive::Undefine,
    Directive::Pragma,
    Directive::Prefix,
    Directive::Run,
    Directive::Verify,
    Directive::XFail,
};

inline constexpr std::array DirectiveNames{
    "*"sv,
    "+"sv,
    "!"sv,
    "!*"sv,
    "!+"sv,
    "re*"sv,
    "re+"sv,
    "re!"sv,
    "re!*"sv,
    "re!+"sv,
    "b"sv,
    "d"sv,
    "e"sv,
    "u"sv,
    "p"sv,
    "FCHK-PREFIX"sv,
    "R"sv,
    "V"sv,
    "X"sv,
};

static_assert(
    DirectiveToRegexDirective.size() == +Directive::Count + 1,
    "DirectiveToRegexDirective array is out of sync"
);

static_assert(
    DirectiveNames.size() == +Directive::Count + 1,
    "DirectiveNames array is out of sync"
);

class Regex {
    std::string raw;
    void* re_ptr{};
    void* data_ptr{};

public:
    struct Exception : std::exception {
        std::string message;

        template <typename... Args>
        explicit Exception(std::format_string<Args...> fmt, Args&&... args)
            : message(std::format(fmt, std::forward<Args>(args)...)) {}
    };

    ~Regex() noexcept;

    Regex(const Regex&) = delete;
    Regex& operator=(const Regex&) = delete;

    Regex(Regex&& other) noexcept
        : re_ptr(std::exchange(other.re_ptr, nullptr)),
          data_ptr(std::exchange(other.data_ptr, nullptr)) {}

    Regex& operator=(Regex&& other) noexcept {
        std::swap(re_ptr, other.re_ptr);
        std::swap(data_ptr, other.data_ptr);
        return *this;
    }

    /// Create a new regular expression.
    ///
    /// This constructor is explicit because it may throw.
    ///
    /// \param C Context for issuing warnings.
    /// \param pattern The pattern to match.
    /// \throw Regex::Exception if the pattern is invalid.
    explicit Regex(Context& C, std::string pattern);

    /// Match the regular expression against a string.
    ///
    /// \param str The string to match.
    /// \param flags Flags to pass to the regex engine.
    /// \return Whether the match succeeded.
    bool operator()(std::string_view str, u32 flags) const noexcept { return match(str, flags); }

    /// Match the regular expression against a string.
    ///
    /// \param str The string to match.
    /// \param flags Flags to pass to the regex engine.
    /// \return Whether the match succeeded.
    bool match(std::string_view str, u32 flags) const noexcept;

    /// Get match data pointer.
    [[nodiscard]] auto data() const noexcept -> void* { return data_ptr; }

    /// Get regular expression pointer.
    [[nodiscard]] auto ptr() const noexcept -> void* { return re_ptr; }

    /// Get raw text.
    [[nodiscard]] auto raw_text() const noexcept -> std::string_view { return raw; }
};

/// Entry in the environment.
struct EnvEntry {
    std::string name;
    std::string value;
    bool literal;
    bool local;
};

/// Environment used by environment regexes.
using Environment = std::vector<EnvEntry>;

/// Regular expression together with an environment. Prefer to
/// use Regex over this if there are no named captures as it will
/// be faster in the general case.
struct EnvironmentRegex {
    std::string re_str;
    std::unordered_set<char> literal_chars;
    StringSet defined_captures;

    /// Create a new regular expression.
    ///
    /// \param pattern The pattern to match.
    /// \param literal_chars Characters to be treated as literal.
    /// \param captype Whether the ‘typed captures’ feature (e.g. $name:type) is enabled.
    /// \param builtins_enabled Whether builtins are enabled.
    EnvironmentRegex(
        std::string pattern,
        std::unordered_set<char> literal_chars,
        bool captype,
        bool builtins_enabled
    );

    /// Substitute environment variables in the string.
    auto substitute_vars(Context& ctx, Location loc, const Environment& env) -> std::string;
};

/// A check that needs to be, well, checked.
struct Check {
    using Data = std::variant<std::string, Regex, EnvironmentRegex>;

    Directive dir;
    Data data;
    Location loc;

    bool is_negative_check() const {
        switch (dir) {
            default: return false;
            case Directive::CheckNotSame:
            case Directive::CheckNotAny:
            case Directive::CheckNotNext:
            case Directive::RegexCheckAny:
            case Directive::RegexCheckNext:
            case Directive::RegexCheckNotSame:
            case Directive::RegexCheckNotAny:
            case Directive::RegexCheckNotNext:
                return true;
        }
    }
};

namespace detail {
class Matcher;
}

struct DiagsHandler {
    enum struct Kind {
        Note,    ///< Informational note.
        Warning, ///< Warning, but no hard error.
        Error,   ///< Hard error.
    };

    enum struct Colour {
        Yellow,
        Red,
        Green,
        Blue,
        Default,
        Reset,
    };

    using enum Colour;

    /// Whether to enable colours.
    bool enable_colours = true;

    /// What stream to print to.
    FILE* stream = stderr;

    virtual ~DiagsHandler() = default;
    virtual void write(std::string_view text);
    virtual auto get_error_handler() -> std::function<bool(std::string&&)> { return nullptr; }

    /// Get the colour of a diagnostic.
    auto colour(Kind kind) -> std::string_view {
        using Kind = Kind;
        if (not enable_colours) return "";
        switch (kind) {
            case Kind::Warning: return colour(Yellow);
            case Kind::Note: return colour(Green);
            case Kind::Error: return colour(Red);
            default: return "";
        }
    }

    /// Get the ANSI escape sequence for a colour.
    auto colour(Colour c, bool bold = true) -> std::string_view {
        if (not enable_colours) return "";
        switch (c) {
            case Yellow: return bold ? "\033[1;33m"sv : "\033[0;33m"sv;
            case Red: return bold ? "\033[1;31m"sv : "\033[0;31m"sv;
            case Green: return bold ? "\033[1;32m"sv : "\033[0;32m"sv;
            case Blue: return bold ? "\033[1;34m"sv : "\033[0;34m"sv;
            case Default: return bold ? "\033[m\033[1m"sv : "\033[m"sv;
            case Reset: return "\033[m"sv;
        }
        Unreachable("Invalid colour");
    }

    template <typename... Args>
    void diag(Context& ctx, Kind k, Location where, std::format_string<Args...> fmt, Args&&... args) {
        report(ctx, k, where, std::format(fmt, std::forward<Args>(args)...));
    }

    template <typename... Args>
    void print(std::format_string<Args...> fmt, Args&&... args) {
        write(std::format(fmt, std::forward<Args>(args)...));
    }

    // This calls `write()` to print the actual error.
    void report(Context* ctx, Kind k, Location where, std::string_view text) {
        report_impl(ctx, k, where, text, false);
    }

    void report_no_line(Context& ctx, Kind k, Location where, std::string_view text) {
        report_impl(&ctx, k, where, text, true);
    }

private:
    void report_impl(Context* ctx, Kind k, Location where, std::string_view text, bool no_line);
};

class Context {
    friend DiagsHandler;

    /// State associated with a particular prefix.
    struct PrefixState {
        /// The prefix for this state.
        std::string prefix;

        /// The checks that we have to perform.
        std::vector<Check> checks;

        /// Enabled pragmas.
        StringMap<bool> pragmas;

        /// Characters to be treated as literal in regexes.
        std::unordered_set<char> literal_chars;
    };

    /// A single test program.
    struct Test {
        std::string_view run_directive;
        PrefixState* state;
        bool verify_only;
        bool xfail;
    };

    /// Diagnostics handler.
    std::shared_ptr<DiagsHandler> dh;

    /// Checks
    File check_file;

    /// State for each prefix. 0 is the default one.
    std::deque<PrefixState> states_by_prefix;

    /// Programs to run and associated checks.
    std::vector<Test> run_directives;

    /// Default pragmas set on the command line.
    StringMap<bool> default_pragmas;

    /// Default literal chars set on the command line.
    std::unordered_set<char> default_literal_chars;

    /// Definitions for run directives.
    StringMap<std::string> definitions;

    /// Stop on an error.
    bool abort_on_error;

    /// Print verbose error messages.
    bool verbose;

    /// Enable builtin magic variables.
    bool enable_builtins;

    /// Error flag.
    mutable bool has_error = false;
    mutable bool has_diag = false;

public:
    friend Location;
    friend detail::Matcher;

    /// Run fchk’s main function.
    static int RunMain(std::shared_ptr<DiagsHandler> dh, int argc, char** argv);

    /// Create a context for running checks.
    explicit Context(
        std::shared_ptr<DiagsHandler> dh,
        std::string check,
        fs::Path check_name = "<input>",
        std::string prefix = {},
        StringMap<bool> pragmas = {},
        std::unordered_set<char> literal_chars = {},
        std::span<const std::string> defines = {},
        bool abort_on_error = false,
        bool verbose = false,
        bool enable_builtins = true
    );

    /// Check if builtins are enabled.
    [[nodiscard]] bool BuiltinsEnabled() const { return enable_builtins; }

    /// Get the location of a string view in a file.
    ///
    /// Since this only works if this is exactly a string view taken
    /// from the file, we ensure that only string views can be passed
    /// in.
    template <std::same_as<std::string_view> SV>
    [[nodiscard]] auto LocationIn(SV sv, File& file) const -> Location {
        auto start = sv.data() - file.contents.data();
        return Location{u32(start), u16(sv.size()), &file};
    }

    /// Entry point.
    int Run();

    /// Diagnostics.
    void Diag(DiagsHandler::Kind k, Location where, std::string_view message) {
        dh->report(this, k, where, message);
    }

    template <typename... Args>
    void Error(Location where, std::format_string<Args...> fmt, Args&&... args) {
        Diag(DiagsHandler::Kind::Error, where, std::format(fmt, std::forward<Args>(args)...));
    }

    template <typename... Args>
    void Note(Location where, std::format_string<Args...> fmt, Args&&... args) {
        Diag(DiagsHandler::Kind::Note, where, std::format(fmt, std::forward<Args>(args)...));
    }

    template <typename... Args>
    void NoteNoLine(Location where, std::format_string<Args...> fmt, Args&&... args) {
        dh->report_no_line(*this, DiagsHandler::Kind::Note, where, std::format(fmt, std::forward<Args>(args)...));
    }

    template <typename... Args>
    void Warning(Location where, std::format_string<Args...> fmt, Args&&... args) {
        Diag(DiagsHandler::Kind::Warning, where, std::format(fmt, std::forward<Args>(args)...));
    }

    template <typename... Args>
    void VerboseLog(std::format_string<Args...> fmt, Args&&... args) {
        if (verbose) dh->write(std::format(fmt, std::forward<Args>(args)...));
    }

private:
    /// Collect all directives that start with a prefix.
    void CollectDirectives(PrefixState& state);

    /// Create a default prefix state.
    auto CreatePrefixState(std::string prefix) -> PrefixState*;

    /// Run a test.
    ///
    /// \return True if the test succeeded.
    void RunTest(Test& t);
};
}

#endif // FCHK_CORE_HH
