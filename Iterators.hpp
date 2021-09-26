/**
 * @author tim Luchterhand
 * @date 10.09.21
 * @brief This file contains the definitions of Python-like zip- and enumerate-functions. They can be used in range
 * based for-loops to loop over multiple ranges at the same time, or to index a range while looping respectively.
 */

#ifndef ITERATORTOOLS_ITERATORS_HPP
#define ITERATORTOOLS_ITERATORS_HPP

#include <tuple>

namespace iterators {
    namespace impl {
        template<bool Cond, typename T>
        struct reference_if {
            using type = T;
        };

        template<typename T>
        struct reference_if<true, T> {
            using type = std::add_lvalue_reference_t<T>;
        };

        template<bool Cond, typename T>
        using reference_if_t = typename reference_if<Cond, T>::type;

        template<typename ...Iterable>
        struct zip_ {
            using ContainerTuple = std::tuple<Iterable...>;

            template<typename ...Container>
            explicit zip_(Container &&...containers) : containers(std::forward<Container>(containers)...) {}

            class ZipIterator {
                using IteratorTuple = std::tuple<decltype(std::begin(
                        std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
                using ValueTuple = std::tuple<decltype(*std::begin(
                        std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
            public:
                enum class Construct {
                    End
                };

                explicit ZipIterator(ContainerTuple &containers) :
                        iterators(std::apply([](auto &&...c) { return std::tuple(std::begin(c)...); }, containers)) {}

                ZipIterator(ContainerTuple &containers, Construct) :
                        iterators(std::apply([](auto &&...c) { return std::tuple(std::end(c)...); }, containers)) {}

                ZipIterator &operator++() {
                    std::apply([](auto &&...it) { (++it, ...); }, iterators);
                    return *this;
                }

                bool operator==(const ZipIterator &other) const {
                    return equal(other.iterators);
                }

                bool operator!=(const ZipIterator &other) const {
                    return !(*this == other);
                }

                auto operator*() {
                    return std::apply([](auto &&...it) { return ValueTuple(*it...); }, iterators);
                }

            private:
                template<std::size_t N = 0>
                [[nodiscard]] constexpr bool equal(const IteratorTuple &other) const {
                    if constexpr (N == std::tuple_size_v<IteratorTuple>) {
                        return false;
                    } else {
                        if (std::get<N>(iterators) == std::get<N>(other)) {
                            return true;
                        } else {
                            return equal<N + 1>(other);
                        }
                    }
                }

                IteratorTuple iterators;
            };

            ZipIterator begin() {
                return ZipIterator(containers);
            }

            ZipIterator end() {
                return ZipIterator(containers, ZipIterator::Construct::End);
            }

        private:
            ContainerTuple containers;
        };

        template<typename T>
        struct CounterIterator {
            static_assert(std::is_integral_v<T> && !std::is_floating_point_v<T>);

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
            explicit CounterContainer(std::size_t start) : start(start) {}

            [[nodiscard]] CounterIterator<std::size_t> begin() const {
                return CounterIterator<std::size_t>(start, std::numeric_limits<std::size_t>::max());
            }

            [[nodiscard]] static constexpr CounterIterator<std::size_t> end() {
                return CounterIterator<std::size_t>(std::numeric_limits<std::size_t>::max(),
                                                    std::numeric_limits<std::size_t>::max());
            }

        private:
            std::size_t start;
        };
    }

    /**
     * Function that can be used in range based loops to emulate the zip iterator from python.
     * As in python: if the passed containers have different lengths, the container with the least items decides
     * the overall range
     * @tparam Iterable Container types that support iteration
     * @param iterable Arbitrary number of containers
     * @return zip-container class that provides begin and end members to be used in range based for-loops
     */
    template<typename ...Iterable>
    auto zip(Iterable &&...iterable) {
        return impl::zip_<Iterable...>(std::forward<Iterable>(iterable)...);
    }

    /**
     * Zip variant that does not allow manipulation of the container elements
     * @tparam Iterable Container types that support iteration
     * @param iterable Arbitrary number of containers
     * @return zip-container class that provides begin and end members to be used in range based for-loops.
     */
    template<typename ...Iterable>
    auto const_zip(Iterable &&...iterable) {
        return impl::zip_<impl::reference_if_t<std::is_lvalue_reference_v<Iterable>,
                std::add_const_t<std::remove_reference_t<Iterable>>>...>(std::forward<Iterable>(iterable)...);
    }

    /**
     * Function that can be used in range based loops to emulate the enumerate iterator from python.
     * @tparam Container Container type that supports iteration
     * @param container Arbitrary number of containers
     * @param start Optional index offset (default 0)
     * @return zip-container class that provides begin and end members to be used in range based for-loops.
     */
    template<typename Container>
    auto enumerate(Container &&container, std::size_t start = 0) {
        return zip(impl::CounterContainer(start), std::forward<Container>(container));
    }

    /**
     * enumerate variant that does not allow manipulation of the container elements
     * @tparam Container Container type that supports iteration
     * @param container Arbitrary number of containers
     * @param start Optional index offset (default 0)
     * @return zip-container class that provides begin and end members to be used in range based for-loops.
     */
    template<typename Container>
    auto const_enumerate(Container &&container, std::size_t start = 0) {
        return const_zip(impl::CounterContainer(start), std::forward<Container>(container));
    }

}

#endif //ITERATORTOOLS_ITERATORS_HPP
