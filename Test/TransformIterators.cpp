#include <gtest/gtest.h>
#include <array>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <map>
#include "Iterators.hpp"
#include "utils.hpp"

TEST(TransformIterators, transform_results) {
    std::array numbers{1, 2, 3, 4};
    std::array results{1, 4, 9, 16};
    using iterators::const_zip;
    using iterators::transform;

    auto square = [](const auto &num) {
        return num * num;
    };

    for (auto [squared, expected] : const_zip(transform(numbers, square), results)) {
        EXPECT_EQ(squared, expected);
    }
}

TEST(TransformIterators, reference_result) {
    using iterators::transform;
    std::unordered_map<int, std::string> map{{1, "1"}, {2, "2"}, {3, "3"}};
    std::unordered_map<int, std::string> results{{1, "1a"}, {2, "2a"}, {3, "3a"}};
    auto values = [](auto &pair) -> auto & {
        return pair.second;
    };

    EXPECT_TRUE(std::is_lvalue_reference_v<decltype(*transform(map, values).begin())>);
    for (auto &string : transform(map, values)) {
        string += "a";
    }

    EXPECT_EQ(map, results);
}

TEST(TransformIterators, constness) {
    using iterators::transform;
    std::unordered_map<int, std::string> map{{1, "1"}, {2, "2"}, {3, "3"}};
    const auto &constMap = map;
    auto values = [](auto &pair) -> auto & {
        return pair.second;
    };

    auto t = transform(map, values);
    auto constT = transform(constMap, values);
    EXPECT_FALSE(std::is_const_v<std::remove_reference_t<decltype(*t.begin())>>);
    EXPECT_TRUE(std::is_const_v<std::remove_reference_t<decltype(*constT.begin())>>);
}

TEST(TransformIterators, deref_member_access) {
    using iterators::transform;
    std::map<int, std::string> map{{1, "1"}, {2, "2"}, {3, "3"}};
    auto identity = [](auto &a) -> auto & {return a;};
    auto it = transform(map, identity).begin();
    EXPECT_EQ(it->first, 1);
    EXPECT_EQ(it->second, "1");
}