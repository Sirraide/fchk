#include <core.hh>

using namespace fchk;

int main(int argc, char** argv) {
    return Context::RunMain(std::make_shared<DiagsHandler>(), argc, argv);
}
