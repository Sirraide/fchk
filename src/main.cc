#include <core.hh>
#include <utils.hh>

int main(int argc, char** argv) {
    auto dh = std::make_shared<DiagsHandler>();
    return Context::RunMain(std::make_shared<DiagsHandler>(), argc, argv);
}
