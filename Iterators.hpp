//
// Created by tim on 10.09.21.
//

#ifndef ITERATORTOOLS_ITERATORS_HPP
#define ITERATORTOOLS_ITERATORS_HPP

#include <iostream>
#include <tuple>

namespace iterators {
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

    /**
     * Class that can be used in range based loops to emulate the zip_ iterator from python.
     * As in python: if the passed containers have different lengths, the container with the least items decides
     * the overall range
     * @tparam Readonly Whether the elements in the containers can be manipulated
     * @tparam Iterable Container types that support iteration
     */
    template<typename ...Iterable>
    struct zip_ {
        using ContainerTuple = std::tuple<Iterable...>;
        /**
         * Ctor
         * @param containers Arbitrary number of iterable containers
         */
        template<typename ...Container>
        explicit zip_(Container &&...containers) : containers(std::forward<Container>(containers)...) {
            std::cout << "zip ctor" << std::endl;
        }

        ~zip_() {
            std::cout << "zip dtor" << std::endl;

        }

        class ZipIterator {
            using IteratorTuple = std::tuple<decltype(std::begin(std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
            using ValueTuple = std::tuple<decltype(*std::begin(std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
        public:
            enum class Construct {
                End
            };

            explicit ZipIterator(ContainerTuple &containers) :
                    iterators(std::apply([](auto &&...c) { return std::tuple(std::begin(c)...); }, containers)) {
            }

            ZipIterator(ContainerTuple &containers, Construct) :
                    iterators(std::apply([](auto &&...c) { return std::tuple(std::end(c)...); }, containers)) {
            }

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
        ContainerTuple containers;
    };

    template<typename ...Iterable>
    auto zip(Iterable &&...iterable) {
        return zip_<Iterable...>(std::forward<Iterable>(iterable)...);
    }

    template<typename ...Iterable>
    auto const_zip(Iterable &&...iterable) {
        return zip_<reference_if_t<std::is_lvalue_reference_v<Iterable>, std::add_const_t<std::remove_reference_t<Iterable>>>...>(
               std::forward<Iterable>(iterable)...);
    }


    namespace impl {
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

        struct enumerate_storage {
            explicit enumerate_storage(std::size_t start) : counter(start) {}
        protected:
            CounterContainer counter;
        };
    }


    /**
     * Class that can be used in range based loops to emulate the enumerate iterator from python.
     * @tparam Container Container type that supports iteration
     * @tparam Readonly Whether the elements in the container can be manipulated
     */
     /*
    template<typename Container, bool Readonly = false>
    struct enumerate : public impl::enumerate_storage, zip_<Readonly, impl::CounterContainer, Container> {
        explicit enumerate(Container &c, std::size_t start = 0) :
                impl::enumerate_storage(start), zip_<Readonly, impl::CounterContainer, Container>(counter, c) {}
    };*/

    /**
     * enumerate specialization that does not allow manipulation of the container elements
     * @tparam Container Container type that supports iteration
     */
     /*
    template<typename Container>
    struct const_enumerate : public enumerate<Container, true> {
        explicit const_enumerate(Container &c, std::size_t start = 0) : enumerate<Container, true>(c, start) {}
    };*/
}

#endif //ITERATORTOOLS_ITERATORS_HPP
