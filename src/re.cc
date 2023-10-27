#include <core.hh>
#include <re.hh>

#define PCRE2_CODE_UNIT_WIDTH 8
#define PCRE2_STATIC          1
#include <pcre2.h>

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
        buffer.resize(4096);
        auto sz = pcre2_get_error_message(
            err,
            reinterpret_cast<PCRE2_UCHAR8*>(buffer.data()),
            buffer.size()
        );

        if (sz == PCRE2_ERROR_BADDATA) Diag::Warning("PCRE error code is invalid");
        else if (sz == PCRE2_ERROR_NOMEMORY) Diag::Warning("PCRE error buffer is too small to accommodate error message");
        else buffer.resize(usz(sz));
        throw Exception{std::move(buffer)};
    }

    /// JIT-compile the RE, if possible.
    if (pcre2_jit_compile(expr, PCRE2_JIT_COMPLETE) != 0) {
        pcre2_code_free(expr);
        throw Exception("Failed to JIT compile regex");
    }

    re_ptr = expr;
    data_ptr = pcre2_match_data_create_from_pattern(expr, nullptr);
}

bool Regex::match(std::string_view str) const noexcept {
    auto re = reinterpret_cast<pcre2_code*>(re_ptr);
    auto data = reinterpret_cast<pcre2_match_data*>(data_ptr);
    int code = pcre2_match(
        re,
        reinterpret_cast<PCRE2_SPTR8>(str.data()),
        str.size(),
        0,
        PCRE2_ANCHORED,
        data,
        nullptr
    );
    return code >= 0;
}
