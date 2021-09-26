#include <gtest/gtest.h>
#include <vector>
#include <list>
#include <string>
#include <type_traits>
#include "Iterators.hpp"
#include "utils.hpp"

TEST(Iterators, zip_elements) {
    using namespace iterators;
    std::list<std::string> strings{"a", "b", "c"};
    std::vector<int> numbers{1, 2, 3};
    auto sIt = strings.begin();
    auto nIt = numbers.begin();
    for (auto [string, number] : const_zip(strings, numbers)) {
        EXPECT_EQ(string, *sIt);
        EXPECT_EQ(number, *nIt);
        ++sIt; ++nIt;
    }

    EXPECT_EQ(sIt, strings.end());
    EXPECT_EQ(nIt, numbers.end());
}

TEST(Iterators, zip_c_arrays) {
    using namespace iterators;
    int numbers[] = {1, 2, 3};
    const char * strings[] = {"a", "b", "c"};
    auto sIt = std::begin(strings);
    auto nIt = std::begin(numbers);
    for (auto [string, number] : const_zip(strings, numbers)) {
        EXPECT_EQ(string, *sIt);
        EXPECT_EQ(number, *nIt);
        ++sIt; ++nIt;
    }

    EXPECT_EQ(sIt, std::end(strings));
    EXPECT_EQ(nIt, std::end(numbers));
}

TEST(Iterators, zip_inequal_length) {
    using namespace iterators;
    std::list<std::string> strings{"a", "b", "c"};
    std::vector<int> numbers{1, 2, 3, 4, 5};
    auto sIt = std::begin(strings);
    auto nIt = std::begin(numbers);
    for (auto [string, number] : const_zip(strings, numbers)) {
        EXPECT_EQ(string, *sIt);
        EXPECT_EQ(number, *nIt);
        ++sIt; ++nIt;
    }

    EXPECT_EQ(sIt, std::end(strings));
    EXPECT_EQ(*nIt, 4);
}

TEST(Iterators, zip_mutuate) {
    using namespace iterators;
    std::list<std::string> strings{"a", "b", "c"};
    std::vector<int> numbers{1, 2, 3};
    for (auto [string, number] : zip(strings, numbers)) {
        string += std::to_string(number);
        number *= 2;
    }

    std::vector<int> numbersResults{2, 4, 6};
    std::list<std::string> stringsResults{"a1", "b2", "c3"};
    EXPECT_TRUE(std::equal(numbers.begin(), numbers.end(), numbersResults.begin()));
    EXPECT_TRUE(std::equal(strings.begin(), strings.end(), stringsResults.begin()));
}

TEST(Iterators, zip_constness) {
    using namespace iterators;
    std::list<std::string> strings;
    std::vector<int> numbers;
    EXPECT_TRUE(std::is_const_v<std::remove_reference_t<decltype(std::get<0>(*const_zip(strings, numbers).begin()))>>);
    EXPECT_TRUE(std::is_const_v<std::remove_reference_t<decltype(std::get<1>(*const_zip(strings, numbers).begin()))>>);
}

TEST(Iterators, elements_no_copy) {
    using namespace iterators;
    std::vector<MustNotCopy> items;
    items.emplace_back("a");
    items.emplace_back("b");
    items.emplace_back("c");
    for (auto [item] : zip(items)) {
        item.s = "";
    }

    for (auto [item] : zip(std::move(items))) {
        item.s = "";
    }
}

TEST(Iterators, container_no_copy) {
    using namespace iterators;
    MustNotCopyContainer<std::string> strings{"a", "b", "c"};
    MustNotCopyContainer<int> numbers{1, 2, 3};
    for (auto [string, number] : zip(strings, numbers)) {
        number *= 2;
        string += std::to_string(number);
    }
}

TEST(Iterators, enumerate_elements) {
    using namespace iterators;
    std::list<std::string> strings{"a", "b", "c"};
    std::size_t i = 0;
    auto sIt = strings.begin();
    for (auto [index, string] : const_enumerate(strings)) {
        EXPECT_EQ(string, *sIt);
        EXPECT_EQ(index, i++);
        ++sIt;
    }
}

TEST(Iterators, enumerate_offset) {
    using namespace iterators;
    std::list<std::string> strings{"a", "b", "c"};
    std::size_t i = 4;
    auto sIt = strings.begin();
    for (auto [index, string] : const_enumerate(strings, i)) {
        EXPECT_EQ(string, *sIt);
        EXPECT_EQ(index, i++);
        ++sIt;
    }
}

TEST(Iterators, enumerate_mutuate) {
    using namespace iterators;
    std::list<std::string> strings{"a", "b", "c"};
    for (auto [index, string] : enumerate(strings)) {
        string += std::to_string(index);
        index += 2; // should have no effect
    }

    std::list<std::string> stringsResults{"a0", "b1", "c2"};
    EXPECT_TRUE(std::equal(strings.begin(), strings.end(), stringsResults.begin()));
}

TEST(Iterators, enumerate_constness) {
    using namespace iterators;
    std::list<std::string> strings;
    EXPECT_TRUE(std::is_const_v<std::remove_reference_t<decltype(std::get<1>(*const_enumerate(strings).begin()))>>);
}

TEST(Iterators, enumerate_elements_no_copy) {
    using namespace iterators;
    std::vector<MustNotCopy> items;
    items.emplace_back("a");
    items.emplace_back("b");
    items.emplace_back("c");
    for (auto [index, item] : enumerate(items)) {
        item.s += std::to_string(index);
    }

    for (auto [index, item] : enumerate(std::move(items))) {
        item.s += std::to_string(index);
    }
}

TEST(Iterators, enumerate_container_no_copy) {
    using namespace iterators;
    std::list<std::string> strings{"a", "b", "c"};
    for (auto [index, string] : enumerate(strings)) {
        string += std::to_string(index);
    }
}

TEST(Iterators, temporary_container) {
    using namespace iterators;
    std::array expected_values{4, 5, 6};
    for (auto [expected, actual] : zip(expected_values, std::array{4, 5, 6})) {
        EXPECT_EQ(expected, actual);
    }
}

TEST(Iterators, temporary_no_copy) {
    using namespace iterators;
    std::array expected_values{1, 2, 3};
    for (auto [expected, actual] : zip(expected_values, MustNotCopyContainer<int>({1, 2, 3}))) {
        EXPECT_EQ(expected, actual);
    }
}

TEST(Iterators, temporary_lifetime) {
    using namespace iterators;
    std::array expected_values{1, 2, 3};
    bool allowToDie = false;
    for (auto [expected, actual] : zip(expected_values, LifeTimeChecker<int>({1, 2, 3}, allowToDie))) {
        EXPECT_EQ(expected, actual);
        if (actual == 3) { // after the last iteration, the temporary container may be destroyed
            allowToDie = true;
        }
    }
}
