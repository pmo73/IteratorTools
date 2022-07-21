#include <gtest/gtest.h>
#include "Iterators.hpp"

TEST(CounterIterator, basic) {
    using namespace iterators::impl;
    CounterIterator test(0);
    EXPECT_EQ(*test, 0);
    EXPECT_EQ(*++test, 1);
    EXPECT_EQ(*test++, 1);
    EXPECT_EQ(*test, 2);
    EXPECT_EQ(test, CounterIterator(2));
    EXPECT_NE(test, CounterIterator(3));
    EXPECT_EQ(--test, CounterIterator(1));
    EXPECT_EQ(test--, CounterIterator(1));
    EXPECT_EQ(*test, 0);
    EXPECT_EQ(test + 3, CounterIterator(3));
    EXPECT_EQ(3 + test, CounterIterator(3));
    test += 5;
    EXPECT_EQ(*test, 5);
    EXPECT_EQ(test - 2, CounterIterator(3));
    test -= 3;
    EXPECT_EQ(*test, 2);
    EXPECT_EQ(test[4], 6);
}

TEST(CounterIterator, sentinel) {
    using namespace iterators::impl;
    CounterIterator test(3);
    EXPECT_NE(test, Unreachable{});
    test += 1000;
    EXPECT_NE(test, Unreachable{});
    EXPECT_LT(test, Unreachable{});
    EXPECT_LE(test, Unreachable{});
}
