#ifndef BIDIRECTIONALMAP_MUSTNOTCOPY_HPP
#define BIDIRECTIONALMAP_MUSTNOTCOPY_HPP

#include <string>
#include <gtest/gtest.h>

class MustNotCopy {
public:
    MustNotCopy(std::string string);
    MustNotCopy(const char *chars);
    MustNotCopy() = default;
    MustNotCopy(const MustNotCopy &other);

    MustNotCopy(MustNotCopy &&) = default;
    MustNotCopy &operator=(const MustNotCopy &) = default;
    MustNotCopy &operator=(MustNotCopy &&) = default;
    ~MustNotCopy() = default;

    operator std::string () {
        return s;
    }

    bool operator==(const MustNotCopy &other) const;

    bool operator!=(const MustNotCopy &other) const;

    std::string s;
};


template<typename T>
struct MustNotCopyContainer {
    MustNotCopyContainer(std::initializer_list<T> init) : values(init.begin(), init.end()) {}

    MustNotCopyContainer(const MustNotCopyContainer &) {
        EXPECT_TRUE(false) << "Container was copied";
    }

    MustNotCopyContainer(MustNotCopyContainer &&other) = default;
    MustNotCopyContainer &operator=(const MustNotCopyContainer &) = default;
    MustNotCopyContainer &operator=(MustNotCopyContainer &&) = default;
    ~MustNotCopyContainer() = default;

    auto begin() {
        return values.begin();
    }

    auto begin() const {
        return values.begin();
    }

    auto end() {
        return values.end();
    }

    auto end() const {
        return values.end();
    }

    std::vector<T> values;
};


#endif //BIDIRECTIONALMAP_MUSTNOTCOPY_HPP
