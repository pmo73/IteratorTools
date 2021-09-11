//
// Created by tim on 10.09.21.
//

#ifndef ITERATORTOOLS_ITERATORS_HPP
#define ITERATORTOOLS_ITERATORS_HPP

#include <tuple>

namespace iterators {
    /**
     * Class that can be used in range based loops to emulate the zip iterator from python.
     * As in python: if the passed containers have different lengths, the container with the least items decides
     * the overall range
     * @tparam Readonly Whether the elements in the containers can be manipulated
     * @tparam Iterable Container types that support iteration
     */
    template<bool Readonly = false, typename ...Iterable>
    struct zip {
        /**
         * Ctor
         * @param containers Arbitrary number of iterable containers
         */
        explicit zip(Iterable &...containers) : containers(containers...) {}

        class ZipIterator {
            using IteratorTuple = std::tuple<decltype(std::begin(
                    std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
            using ValueTuple = std::tuple<decltype(*std::begin(
                    std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
            using cValueTuple = std::tuple<decltype(*std::begin(
                    std::declval<std::add_lvalue_reference_t<std::add_const_t<Iterable>>>()))...>;
        public:
            enum class Construct {
                End
            };

            explicit ZipIterator(std::tuple<Iterable &...> &containers) :
                    iterators(std::apply([](auto &&...c) { return std::tuple(std::begin(c)...); }, containers)),
                    ends(std::apply([](auto &&...c) { return std::tuple(std::end(c)...); }, containers)) {
            }

            ZipIterator(std::tuple<Iterable &...> &containers, Construct) :
                    iterators(std::apply([](auto &&...c) { return std::tuple(std::end(c)...); }, containers)),
                    ends(iterators) {
            }

            ZipIterator &operator++() {
                std::apply([] (auto &&...it) {(++it, ...);}, iterators);
                return *this;
            }

            bool operator==(const ZipIterator &other) const {
                return equal<0>(other.iterators);
            }

            bool operator!=(const ZipIterator &other) const {
                return !(*this == other);
            }

            auto operator*() {
                if constexpr(Readonly) {
                    return std::apply([this] (auto &&...it) { return cValueTuple(*it...); }, iterators);

                } else {
                    return std::apply([this] (auto &&...it) { return ValueTuple(*it...); }, iterators);
                }
            }

        private:
            template<std::size_t N>
            [[nodiscard]] constexpr bool equal(const IteratorTuple &other, bool oneNotEqual = false) const {
                if constexpr(N == std::tuple_size_v<IteratorTuple>) {
                    return !oneNotEqual;
                } else {
                    bool itEqual = std::get<N>(iterators) == std::get<N>(other);
                    if (itEqual && std::get<N>(iterators) == std::get<N>(ends)) {
                        return true;
                    }

                    return equal<N + 1>(other, !itEqual || oneNotEqual);
                }
            }

            IteratorTuple iterators;
            IteratorTuple ends;
        };

        /**
         * Returns a ZipIterator to the beginning of the sequence
         * @return
         */
        ZipIterator begin() {
            return ZipIterator(containers);
        }

        /**
         * Returns a ZipIterator to the past-the-end element of the sequence. Do not dereference!
         * @return
         */
        ZipIterator end() {
            return ZipIterator(containers, ZipIterator::Construct::End);
        }

    private:
        std::tuple<Iterable &...> containers;
    };

    /**
     * zip specialization that does not allow manipulation of the container elements
     * @tparam Iterable Container types that support iteration
     */
    template<typename ...Iterable>
    struct const_zip : public zip<true, Iterable...> {
        explicit const_zip(Iterable &...iterable) : zip<true, Iterable...>(iterable...) {}
    };

    namespace impl {
        template<typename T>
        struct CounterIterator {
            static_assert(std::is_integral_v<T> && ! std::is_floating_point_v<T>);
            explicit constexpr CounterIterator(T begin, T end, T increment = T(1)) :
                counter(begin), max(end), increment(increment) {}

            CounterIterator &operator++() {
                if (counter < max) {
                    counter += increment;
                }

                return *this;
            }

            bool operator==(const CounterIterator &other) const {
                return counter == other.counter;
            }

            bool operator!=(const CounterIterator &other) const {
                return !(*this == other);
            }

            constexpr T operator*() const {
                return counter;
            }

        private:
            T counter;
            T max;
            T increment;
        };

        struct CounterContainer {
            [[nodiscard]] static constexpr CounterIterator<std::size_t> begin() {
                return CounterIterator<std::size_t>(0, std::numeric_limits<std::size_t>::max());
            }

            [[nodiscard]] static constexpr CounterIterator<std::size_t> end() {
                return CounterIterator<std::size_t>(std::numeric_limits<std::size_t>::max(),
                                                    std::numeric_limits<std::size_t>::max());
            }
        };
    }

    /**
     * Class that can be used in range based loops to emulate the enumerate iterator from python.
     * @tparam Container Container type that supports iteration
     * @tparam Readonly Whether the elements in the container can be manipulated
     */
    template<typename Container, bool Readonly = false>
    struct enumerate : public zip<Readonly, impl::CounterContainer, Container> {
        explicit enumerate(Container &c, impl::CounterContainer ctr = impl::CounterContainer{}) :
            zip<Readonly, impl::CounterContainer, Container>(ctr, c) {}
    };

    /**
     * enumerate specialization that does not allow manipulation of the container elements
     * @tparam Container Container type that supports iteration
     */
    template<typename Container>
    struct const_enumerate : public enumerate<Container, true> {
        explicit const_enumerate(Container &c) : enumerate<Container, true>(c) {}
    };
}

#endif //ITERATORTOOLS_ITERATORS_HPP
