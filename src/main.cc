#include <clopts.hh>
#include <core.hh>
#include <utils.hh>

namespace detail {
using namespace command_line_options;
using options = clopts< // clang-format off
    option<"-p", "Check prefix to use">,
    multiple<option<"-l", "Treat character(s) as literal">>,
    multiple<option<"-P", "Set a pragma">>,
    multiple<option<"-D", "Define a constant that can be used in 'R' directives">>,
    flag<"-a", "Abort on the first failed check">,
    flag<"-v", "Show more verbose error messages">,
    positional<"checkfile", "File containing the check directives", file<>, true>,
    help<>
>; // clang-format on
} // namespace detail

int main(int argc, char** argv) {
    auto opts = detail::options::parse(argc, argv);

    /// User-provided prefix may not be empty.
    if (auto pre = opts.get<"-p">(); pre and Trim(*pre).empty())
        Diag::Fatal("Prefix may not be empty");

    /// Collect pragmas.
    utils::Map<std::string, bool> pragmas;
    for (auto v : *opts.get<"-P">()) pragmas[v] = true;

    /// Collect literal chars.
    std::unordered_set<char> literal_chars;
    for (auto& v : *opts.get<"-l">())
        for (auto c : v)
            literal_chars.insert(c);

    Context ctx{
        std::move(opts.get<"checkfile">()->contents),
        std::move(opts.get<"checkfile">()->path),
        opts.get_or<"-p">(""),
        std::move(pragmas),
        std::move(literal_chars),
        *opts.get<"-D">(),
        opts.get<"-a">(),
        opts.get<"-v">(),
    };

    return ctx.Run();
}
