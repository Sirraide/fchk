#ifndef FCHK_RE_HH
#define FCHK_RE_HH

#include <expected>
#include <utility>
#include <utils.hh>

class Regex {
    void* re_ptr{};
    void* data_ptr{};

public:
    struct Exception : std::exception {
        std::string message;
        explicit Exception(std::string message) : message(std::move(message)) {}
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
    /// \param pattern The pattern to match.
    /// \throw Regex::Exception if the pattern is invalid.
    Regex(std::string_view pattern);

    /// Match the regular expression against a string.
    bool operator()(std::string_view str) const noexcept { return match(str); }

    /// Match the regular expression against a string.
    bool match(std::string_view str) const noexcept;
};

#endif // FCHK_RE_HH
