#include <clopts.hh>
#include <core.hh>
#include <utils.hh>

namespace detail {
using namespace command_line_options;
using options = clopts< // clang-format off
    option<"-p", "Check prefix to use", std::string>,
    positional<"checkfile", "File containing the check directives", file<>, true>,
    help<>
>; // clang-format on
} // namespace detail

int main(int argc, char** argv) {
    auto opts = detail::options::parse(argc, argv);

    /// User-provided prefix may not be empty.
    if (auto pre = opts.get<"-p">(); pre and Trim(*pre).empty())
        die("Prefix may not be empty");

/*    /// Read stdin if no -f option was provided.
    std::string input;
    static constexpr usz bufsize = 4'096;
    for (;;) {
        input.resize(input.size() + bufsize);
        auto read = std::fread(input.data() + input.size() - bufsize, 1, bufsize, stdin);
        if (read < bufsize) input.resize(input.size() - (bufsize - read));
        if (std::ferror(stdin)) die("Error reading stdin: {}", std::strerror(errno));
        if (std::feof(stdin)) break;
    }*/

    Context ctx{
        std::move(opts.get<"checkfile">()->contents),
        std::move(opts.get<"checkfile">()->path),
        opts.get_or<"-p">(""),
    };

    return ctx.Run();
}
