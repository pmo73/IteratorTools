#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include "Iterators.hpp"

struct NoCopy {
    explicit NoCopy(std::string s) : s(std::move(s)) {}
    NoCopy(const NoCopy &other) : s(other.s) {
        std::cout << "COPY" << std::endl;
    }

    NoCopy &operator=(const NoCopy &other) {
        std::cout << "COPY" << std::endl;
        s = other.s;
        return *this;
    }

    NoCopy(NoCopy &&other) noexcept : s(std::move(other.s)) {
        std::cout << "MOVE" << std::endl;
    }

    NoCopy &operator=(NoCopy &&other)  noexcept {
        std::cout << "MOVE" << std::endl;
        s = std::move(other.s);
        return *this;
    }

    ~NoCopy() = default;

    std::string s;
};

std::ostream &operator<<(std::ostream &os, const NoCopy &nc) {
    os << nc.s;
    return os;
}

int main() {
    int a[] = {1, 2, 3, 4};
    std::vector<NoCopy> ncs = {NoCopy("ui"), NoCopy("uiui"), NoCopy("uiuiui")};
    const char *b[] = {"a", "b", "container", "d"};
    int i = 0;
    std::string s = "bla";
    std::tuple<int&, std::string&> tuple(i, s);
    const auto &[ival, sval] = tuple;
    sval = "asd";
    std::cout << sval << std::endl;
    for (auto [num, string, nc] : iterators::zip(a, b, ncs)) {
        num *= 2;
        std::cout << num << " " << string << " " << nc << std::endl;
    }



    for (const auto &[index, nc] : iterators::enumerate(ncs)) {
        nc.s = "empty";
        std::cout << index << " " << nc << std::endl;
    }
    return 0;
}
