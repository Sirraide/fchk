#include <base/Text.hh>
#include <catch2/catch_all.hpp>
#include <catch2/catch_test_macros.hpp>
#include <core.hh>
#include <errs.hh>

using namespace Catch::literals;
using namespace std::literals;
using namespace fchk;

using SV = std::string_view;

struct TestDiagsHandler : DiagsHandler {
    std::string output;

    TestDiagsHandler() {
        enable_colours = false;
    }

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
void ExpectOutput(
    std::string_view input,
    int ret_val,
    std::string_view output,
    std::string prefix = "#"
) {
    auto dh = std::make_shared<TestDiagsHandler>();
    Context ctx{dh, std::string{stream(input).trim().text()}, "<input>", std::move(prefix)};
    auto res = ctx.Run();
    CHECK(res == ret_val);
    CHECK(stream(dh->output).trim().text() == stream(output).trim().text());
}

void ExpectOutput2(
    std::string input,
    std::string_view checks,
    int ret_val,
    std::string_view output
) {
    utils::ReplaceAll(input, "\n", "\\n"); // Escape newlines so this is parsed as one line.
    ExpectOutput(std::format("# R echo -e \"{}\"\n{}", utils::Escape(input, true), checks), ret_val, output);
}

void ExpectMatch(std::string in, std::string_view tests) {
    return ExpectOutput2(std::move(in), tests, 0, "");
}

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

TEST_CASE("Complain if no prefix is set") {
    ExpectOutput(
        "",
        1,
        "Error: No prefix provided and no FCHK-PREFIX directive found in check file",
        ""
    );
}

TEST_CASE("Warn on attempted prefix override") {
    ExpectOutput(
        "# FCHK-PREFIX X\n# R true\n",
        0,
        R"(
<input>:1:3: Warning: Conflicting prefix directive 'X' ignored (current prefix is '#')
 1 │ # FCHK-PREFIX X
       ~~~~~~~~~~~
        )"
    );
}

TEST_CASE("V directive prints...") {
    SECTION("nothing on success") {
        ExpectOutput("# V echo foobar\n", 0, "");
    }

    SECTION("the program’s output on error") {
        ExpectOutput("# V echo foobar ; false\n", 1, "foobar");
    }
}

TEST_CASE("%s designates the current file") {
    ExpectOutput("# V echo '%s' ; false", 1, "<input>");
}

TEST_CASE("Prefixed lines not starting with a directive are ignored") {
    ExpectOutput(
        "# R true\n # Really not a directive\n # AlsoNotADirective\n",
        0,
        ""
    );
}

TEST_CASE("Basic matching") {
    auto in = "abcd\n1234\nfoo\nbar";

    ExpectMatch(in,
                R"(
# * abcd
# + 1234
# + foo
# + bar
        )");

    ExpectMatch(in,
                R"(
# * abcd
# * 1234
# * foo
# * bar
        )");

    ExpectMatch(in,
                R"(
# * abcd
# + 1234
# * foo
# + bar
        )");
}

TEST_CASE("Mismatch") {
    ExpectOutput2("abcd", "# * abbd", 1, R"(
<input>:2:5: Error: Expected string not found in input
 2 │ # * abbd
         ~~~~
<input>:1:1: Note: Started matching here
 1 │ abcd
     ~~~~
 1 │ abcd
 2 │
)");
}

TEST_CASE("Checks ignore spaces") {
    ExpectMatch(
        "Lorem ipsum dolor sit amet",
        "# * Lorem ipsum dolor sit amet"
    );

    ExpectMatch(
        "    Lorem   ipsum       dolor        sit amet  ",
        "# * Lorem ipsum dolor sit amet"
    );

    ExpectMatch(
        "Lorem ipsum dolor sit amet",
        "# *    Lorem   ipsum       dolor        sit amet  "
    );
}

TEST_CASE("* can skip lines") {
    ExpectMatch(
        "A\nB\nC\nD",
        R"(
# * B
# * D
        )"
    );
}

TEST_CASE("+ can’t skip lines") {
    ExpectOutput2(
        "A\nB\nC\nD",
        R"(
# * B
# + D
        )",
        1,
        R"(
<input>:4:5: Error: Line does not match expected string
 4 │ # + D
         ~
<input>:3:1: Note: Expected match here
 3 │ C
     ~
 2 │ B
 3 │ C
 4 │ D
 5 │
        )"
    );
}

TEST_CASE("Regular expression matching") {
    ExpectMatch(
        "1234567890\n"
        "asdfghjkl",
        "# re* [0-9]+\n"
        "# re+ [a-z]+"
    );
}

TEST_CASE("Pragma nocap") {
    ExpectMatch(
        "Y\n"
        "() (test) ))((",
        "# p nocap\n"
        "# * Y\n"
        "# re+ () (?:(test)) ))(("
    );
}

TEST_CASE("DuplicatePrefix") {
    ExpectMatch(
        "Y\n"
        "() (test) ))((",
        "# p nocap\n"
        "## p nocap off\n" // This is not a directive because the prefix isn’t followed by a space.
        "# * Y\n"
        "# re+ () (?:(test)) ))(("
    );
}

TEST_CASE("Bug#1") {
    ExpectMatch(
        "Y\n"
        "() (test) ))((",
        "#\n"
        "# p nocap\n"
        "# * Y\n"
        "# re+ () (?:(test)) ))(("
    );
}

TEST_CASE("Bug#2") {
    ExpectMatch(
        "abort at </home/ae/projects/Source/test/CG/assert.src:6:5> __src_assert_fail(s\"a == b\", nil)",
        "# p nocap\n"
        "# p captype\n"
        "# p nolit .\n"
        "# re+   abort at .+ __src_assert_fail(s\"a == b\", nil)"
    );
}

TEST_CASE("Ensure that 'update' works") {
    static constexpr std::string_view path = FCHK_PROJECT_DIR "/test/inputs/update.txt";
    auto dh = std::make_shared<TestDiagsHandler>();
    std::array args {
        "fchk",
        path.data(),
        "--prefix",
        "#",
        "--update",
    };

    // Copy the file contents because who knows what happens to the map if the file changes...
    auto before = std::string(File::Read(path).value().view());
    auto res = Context::RunMain(dh, int(args.size()), const_cast<char**>(args.data()));
    auto after = std::string(File::Read(path).value().view());
    CHECK(res == 0);
    CHECK(before == after);
}
