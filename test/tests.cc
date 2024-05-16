#include <catch2/catch_all.hpp>
#include <catch2/catch_test_macros.hpp>
#include <core.hh>
#include <errs.hh>

using namespace Catch::literals;
using namespace std::literals;

using SV = std::string_view;

struct TestDiagsHandler : DiagsHandler {
    std::string output;
    void write(SV text) override { output += text; }

    auto get_error_handler() -> std::function<bool(std::string&&)> override {
        return [](std::string&& message) -> bool { throw std::runtime_error(message); };
    }
};

/// ===========================================================================
///  Driver Tests
/// ===========================================================================
template <std::convertible_to<SV>... Args>
auto RunDriver(Args&&... args) -> std::pair<int, std::string> {
    std::vector<std::string> arguments = {
        "fchk",
        FCHK_PROJECT_DIR "/test/inputs/nop.txt",
        args...
    };

    std::vector<char*> argv;
    for (auto& arg : arguments) argv.push_back(arg.data());
    auto handler = std::make_shared<TestDiagsHandler>();
    auto res = Context::RunMain(handler, int(argv.size()), argv.data());
    return {res, std::move(handler->output)};
}

template <typename... Args>
void CheckDriverError(SV message, Args... args) {
    auto [res, output] = RunDriver(args...);
    CHECK(res != 0);
    CHECK(output.contains(message));
}

template <typename... Args>
void CheckDriverOk(Args... args) {
    auto [res, output] = RunDriver(args...);
    CHECK(res == 0);
    CHECK(output == "");
}

TEST_CASE("Syntax of '--prefix' option") {
    CheckDriverError(ERR_DRV_PREFIX_OPT_INVALID, "--prefix", "");
    CheckDriverError(ERR_DRV_PREFIX_OPT_INVALID, "--prefix=");
    CheckDriverOk("--prefix", "#");
    CheckDriverOk("--prefix=#");
}

TEST_CASE("Syntax of '-D' option") {
    CheckDriverError(ERR_DRV_D_OPT_INVALID, "-D=as");
    CheckDriverError(ERR_DRV_D_OPT_INVALID, "-D", "test");
    CheckDriverOk("-D", "test=foo", "--prefix=#");
}

/// ===========================================================================
///  FCHK Tests
/// ===========================================================================
/// FIXME: Actually split the big file into separate tests.
TEST_CASE("Tests in big file pass") {
    auto dh = std::make_shared<TestDiagsHandler>();
    auto f = command_line_options::detail::map_file<command_line_options::file<>>(
        FCHK_PROJECT_DIR "/test/inputs/basic.txt"sv,
        dh->get_error_handler()
    );

    Context ctx{dh, f.contents, f.path};
    auto res = ctx.Run();
    CHECK(res == 0);
    CHECK(dh->output == "");
}
