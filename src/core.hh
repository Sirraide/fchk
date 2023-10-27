#ifndef FCHK_CORE_HH
#define FCHK_CORE_HH

#include <functional>
#include <unordered_set>
#include <utility>
#include <utils.hh>
#include <vector>

class Context;

// clang-format off
#define AssertImpl(kind, cond, ...) (cond ? void(0) : \
    ::detail::AssertFail(                             \
        ::detail::AssertKind::kind,                   \
        #cond,                                        \
        __FILE__,                                     \
        __LINE__                                      \
        __VA_OPT__(, fmt::format(__VA_ARGS__))        \
    )                                                 \
)

#define AbortImpl(kind, ...)                    \
    ::detail::AssertFail(                       \
        ::detail::AssertKind::kind,             \
        "",                                     \
        __FILE__,                               \
        __LINE__                                \
        __VA_OPT__(, fmt::format(__VA_ARGS__))  \
    )                                           \

#define Assert(cond, ...) AssertImpl(AK_Assert, cond __VA_OPT__(, __VA_ARGS__))
#define Todo(...) AbortImpl(AK_Todo __VA_OPT__(, __VA_ARGS__))
#define Unreachable(...) AbortImpl(AK_Unreachable __VA_OPT__(, __VA_ARGS__))
// clang-format on

namespace detail {
enum struct AssertKind {
    AK_Assert,
    AK_Todo,
    AK_Unreachable,
};

[[noreturn]] void AssertFail(
    AssertKind k,
    std::string_view condition,
    std::string_view file,
    int line,
    std::string&& message = ""
);
}

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
    fs::path path;
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
/// also updating the DirectiveNames array below.
enum struct Directive {
    CheckAny,
    CheckNext,
    CheckNot,
    RegexCheckAny,
    RegexCheckNext,
    RegexCheckNot,
    Define,
    Prefix,
    Run,

    /// These directives only exist internally.
    InternalCheckEmpty,     ///< Match an empty line.
    InternalCheckNextEmpty, ///< Match the next line as empty.
    InternalCheckNotEmpty,  ///< Match the next line as not empty.
};

inline constexpr std::string_view DirectiveNames[]{
    "*",
    "+",
    "!",
    "re*",
    "re+",
    "re!",
    "d",
    "FCHK-PREFIX",
    "R",
};

class EnvironmentRegex;
class Regex {
    void* re_ptr{};
    void* data_ptr{};

    friend EnvironmentRegex;
protected:
    Regex(void* re_ptr, void* data_ptr) noexcept
        : re_ptr(re_ptr), data_ptr(data_ptr) {}

public:
    struct Exception : std::exception {
        std::string message;

        template <typename... Args>
        explicit Exception(fmt::format_string<Args...> fmt, Args&&... args)
            : message(fmt::format(fmt, std::forward<Args>(args)...)) {}
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
    /// \param pattern The pattern to match.
    /// \throw Regex::Exception if the pattern is invalid.
    explicit Regex(std::string_view pattern);

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
};

/// Regular expression together with an environment. Prefer to
/// use Regex over this if there are no named captures as it will
/// be faster in the general case.
class EnvironmentRegex {
    std::string re_str;
    std::unordered_set<std::string, utils::StrHash, std::equal_to<>> defined_captures;

public:
    /// Create a new regular expression.
    ///
    /// \param pattern The pattern to match.
    EnvironmentRegex(std::string pattern);

    /// Match the regular expression against a string.
    ///
    /// \param str The string to match.
    /// \param env The environment to use and populate with captures.
    /// \param flags Flags to pass to the regex engine.
    /// \throw Regex::Exception if the pattern is invalid.
    /// \return Whether the match succeeded.
    bool operator()(std::string_view str, utils::StrMap& env, u32 flags) const {
        return match(str, env, flags);
    }

    /// Match the regular expression against a string.
    ///
    /// \param str The string to match.
    /// \param env The environment to use and populate with captures.
    /// \param flags Flags to pass to the regex engine.
    /// \param capture_visitor A visitor to invoke on each capture.
    /// \throw Regex::Exception if the pattern is invalid.
    /// \return Whether the match succeeded.
    bool match(
        std::string_view str,
        utils::StrMap& env,
        u32 flags,
        std::function<void(std::string_view, std::string_view)> capture_visitor = {}
    ) const;

    /// Visit all captures that are used but not defined by this regex.
    ///
    /// The \c visitor is invoked on the name of each capture, and the
    /// \c fragment_visitor on each text fragment before, inbetween and
    /// after.
    void visit_captures(
        std::function<void(std::string_view)> visitor,
        std::function<void(std::string_view)> fragment_visitor = {}
    ) const;
};

/// A check that needs to be, well, checked.
struct Check {
    using Data = std::variant<std::string, Regex, EnvironmentRegex>;

    Directive dir;
    Data data;
    Location loc;
};

namespace detail {
class Matcher;
}

struct Diag;
class Context {
    /// Checks
    File check_file;

    /// The checks that we have to perform.
    std::vector<Check> checks;

    /// Programs to execute.
    std::vector<std::string_view> run_directives;

    /// Directive prefix.
    std::string_view prefix;

    /// Error flag.
    mutable bool has_error = false;
    mutable bool has_diag = false;

public:
    friend Location;
    friend Diag;
    friend detail::Matcher;

    Context(
        std::string check,
        fs::path check_name,
        std::string_view prefix = ""
    ) : check_file{std::move(check), std::move(check_name)},
        prefix(prefix) {}

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

private:
    /// Run a test.
    void RunTest(std::string_view test);
};

/// A diagnostic. The diagnostic is issued when the destructor is called.
struct Diag {
    /// Diagnostic severity.
    enum struct Kind {
        None,    ///< Not an error. Do not emit this diagnostic.
        Note,    ///< Informational note.
        Warning, ///< Warning, but no hard error.
        Error,   ///< Hard error. Program is ill-formed.
        FError,  ///< Fatal (system) error. NOT a compiler bug.
        ICError, ///< Compiler bug.
    };

private:
    const Context* ctx;
    Kind kind;
    Location where;
    std::string msg;
    bool print_line = true;

    /// Handle fatal error codes.
    void HandleFatalErrors();

    /// Print a diagnostic with no (valid) location info.
    void PrintDiagWithoutLocation();

public:
    static constexpr u8 ICEExitCode = 17;
    static constexpr u8 FatalExitCode = 18;

    Diag(Diag&& other)
        : ctx(other.ctx), kind(other.kind), where(other.where), msg(std::move(other.msg)) {
        other.kind = Kind::None;
    }

    Diag& operator=(Diag&& other) {
        if (this == &other) return *this;
        ctx = other.ctx;
        kind = other.kind;
        where = other.where;
        msg = std::move(other.msg);
        other.kind = Kind::None;
        return *this;
    }

    /// Create an empty diagnostic.
    explicit Diag()
        : ctx(nullptr), kind(Kind::None), where(), msg() {}

    /// Disallow copying.
    Diag(const Diag&) = delete;
    Diag& operator=(const Diag&) = delete;

    /// The destructor prints the diagnostic, if it hasn’t been moved from.
    ~Diag();

    /// Issue a diagnostic.
    Diag(const Context* ctx, Kind kind, Location where, std::string msg)
        : ctx(ctx), kind(kind), where(where), msg(std::move(msg)) {}

    /// Issue a diagnostic with no location.
    Diag(Kind _kind, std::string&& msg)
        : ctx(nullptr), kind(_kind), where(), msg(std::move(msg)) {}

    /// Issue a diagnostic with a format string and arguments.
    template <typename... Args>
    Diag(
        const Context* ctx,
        Kind kind,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    )
        : Diag{ctx, kind, where, fmt::format(fmt, std::forward<Args>(args)...)} {}

    /// Issue a diagnostic with a format string and arguments, but no location.
    template <typename... Args>
    Diag(Kind kind, fmt::format_string<Args...> fmt, Args&&... args)
        : Diag{kind, fmt::format(fmt, std::forward<Args>(args)...)} {}

    /// Don’t print the source line.
    void no_line() { print_line = false; }

    /// Print this diagnostic now. This resets the diagnostic.
    void print();

    /// Suppress this diagnostic.
    void suppress() { kind = Kind::None; }

    /// Emit a note.
    template <typename... Args>
    static Diag Note(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag{Kind::Note, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a note.
    template <typename... Args>
    static Diag Note(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        return Diag{ctx, Kind::Note, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static Diag Warning(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag{Kind::Warning, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static Diag Warning(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        return Diag{ctx, Kind::Warning, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit an error.
    template <typename... Args>
    static Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag{Kind::Error, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit an error.
    template <typename... Args>
    static Diag Error(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        return Diag{ctx, Kind::Error, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Raise an internal compiler error and exit.
    template <typename... Args>
    [[noreturn]] static void ICE(fmt::format_string<Args...> fmt, Args&&... args) {
        Diag{Kind::ICError, fmt::format(fmt, std::forward<Args>(args)...)};
        std::terminate(); /// Should never be reached.
    }

    /// Raise an internal compiler error at a location and exit.
    template <typename... Args>
    [[noreturn]] static void ICE(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        Diag{ctx, Kind::ICError, where, fmt::format(fmt, std::forward<Args>(args)...)};
        std::terminate(); /// Should never be reached.
    }

    /// Raise a fatal error and exit.
    ///
    /// This is NOT an ICE; instead it is an error that is probably caused by
    /// the underlying system, such as attempting to output to a directory that
    /// isn’t accessible to the user.
    template <typename... Args>
    [[noreturn]] static void Fatal(fmt::format_string<Args...> fmt, Args&&... args) {
        Diag{Kind::FError, fmt::format(fmt, std::forward<Args>(args)...)};
        std::terminate(); /// Should never be reached.
    }
};

/// Helper to parse text from a string.
class Stream {
    using SV = std::string_view;

    SV text;

    /// Yield substring until pos and remove it from text.
    SV yield_until(usz pos, bool remove);

public:
    static constexpr std::string_view Whitespace = " \t\v\f";

    Stream(SV text) : text(text) {}

    /// Get a range of characters.
    ///
    /// If either position is out of bounds, it will be
    /// clamped to the nearest valid position.
    [[nodiscard]] auto operator[](usz start, usz end = std::numeric_limits<usz>::max()) const -> SV;

    /// Get the entire text.
    [[nodiscard]] auto operator*() const -> SV { return text; }

    /// Check if this stream starts with text.
    [[nodiscard]] bool at(SV sv) const { return text.starts_with(sv); }

    /// Check if this stream starts with any of a set of characters.
    [[nodiscard]] bool at_any(SV chars) const { return chars.find_first_of(text.front()) != SV::npos; }

    /// Get the current data pointer.
    [[nodiscard]] auto data() const -> const char* { return text.data(); }

    /// Check if this stream is empty.
    [[nodiscard]] bool empty() const { return text.empty(); }

    /// Fold whitespace into a single space and trim
    /// leading and trailing spaces.
    auto fold_ws() const -> std::string;

    /// Read up to a delimiter.
    ///
    /// If the delimiter is not found, this returns the rest of the string.
    [[nodiscard]] auto read_to(SV delim, bool discard = false) -> SV;

    /// Read up to a delimiter or the end of the string.
    ///
    /// If the delimiter is not found, this returns an empty string.
    [[nodiscard]] auto read_to_or_empty(SV delim, bool discard = false) -> SV;

    /// Read up to any of a set of delimiters.
    [[nodiscard]] auto read_to_any(SV delims, bool discard = false) -> SV;

    /// Read up to any of a set of delimiters.
    [[nodiscard]] auto read_to_any(std::span<SV> delims, bool discard = false) -> SV;

    /// Read up to the next whitespace character.
    [[nodiscard]] auto read_to_ws(bool discard = false) -> SV;

    /// Read while a condition is true.
    template <typename Predicate>
    [[nodiscard]] auto read_while(Predicate pred, bool discard = false) -> SV {
        usz pos = 0;
        while (not text.empty() and pred(text[pos])) pos++;
        return yield_until(pos, discard);
    }

    /// Get the size of the stream.
    [[nodiscard]] auto size() const -> usz { return text.size(); }

    /// Skip n characters.
    auto skip(usz n) -> Stream&;

    /// Skip until a delimiter.
    auto skip_to(SV delim) -> Stream&;

    /// Skip until any of a set of delimiters.
    auto skip_to_any(SV delims) -> Stream&;

    /// Skip until any of a set of delimiters.
    auto skip_to_any(std::span<SV> delims) -> Stream&;

    /// Skip to the next whitespace character.
    auto skip_to_ws() -> Stream&;

    /// Skip whitespace, not including line breaks.
    auto skip_ws() -> Stream&;
};

#endif // FCHK_CORE_HH
