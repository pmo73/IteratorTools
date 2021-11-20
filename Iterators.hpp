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

            template<bool End = false>
            class ZipIterator {
                friend ZipIterator<!End>;
                template<typename Container>
                using IteratorReference = std::add_lvalue_reference_t<decltype(std::begin(
                        std::declval<std::add_lvalue_reference_t<Container>>()))>;
                using IteratorTuple = std::tuple<decltype(std::begin(
                        std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
                using IteratorSentinelTuple = std::tuple<decltype(std::end(
                        std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
                using ValueTuple = std::tuple<decltype(*std::begin(
                        std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
            public:
                explicit ZipIterator(ContainerTuple &containers) : iterators(create(containers)) {}

                ZipIterator &operator++() noexcept((noexcept(++std::declval<IteratorReference<Iterable>>()) && ...)) {
                    std::apply([](auto &&...it) { (++it, ...); }, iterators);
                    return *this;
                }

                template<bool B>
                constexpr bool operator==(const ZipIterator<B> &other) const noexcept(noexcept(this->equal(other))) {
                    return equal(other);
                }

                template<bool B>
                constexpr bool operator!=(const ZipIterator<B> &other) const noexcept(noexcept(*this == other)) {
                    return !(*this == other);
                }

                auto operator*() noexcept((noexcept(*std::declval<IteratorReference<Iterable>>()) && ...)) {
                    return std::apply([](auto &&...it) { return ValueTuple(*it...); }, iterators);
                }

            private:
                auto create(ContainerTuple &containerTuple) {
                    if constexpr(End) {
                        return std::apply([](auto &&...c) { return std::tuple(std::end(c)...); }, containerTuple);
                    } else {
                        return std::apply([](auto &&...c) { return std::tuple(std::begin(c)...); }, containerTuple);
                    }
                }

                template<bool B, std::size_t N = 0>
                [[nodiscard]] constexpr bool equal(const ZipIterator<B> &other) const noexcept((noexcept(
                        std::declval<IteratorReference<Iterable>>() ==
                        std::declval<IteratorReference<Iterable>>()) && ...)) {
                    if constexpr (N == std::tuple_size_v<IteratorTuple>) {
                        return false;
                    } else {
                        if (std::get<N>(iterators) == std::get<N>(other.iterators)) {
                            return true;
                        } else {
                            return equal<B, N + 1>(other);
                        }
                    }
                }

                std::conditional_t<End, IteratorSentinelTuple, IteratorTuple> iterators;
            };

            ZipIterator<false> begin() {
                return ZipIterator<false>(containers);
            }

            ZipIterator<true> end() {
                return ZipIterator<true>(containers);
            }

        private:
            ContainerTuple containers;
        };

        struct Unreachable {};

        template<typename T>
        struct CounterIterator {
            static_assert(std::is_integral_v<T> && !std::is_floating_point_v<T>);

            explicit constexpr CounterIterator(T begin, T increment = T(1)) noexcept:
                    counter(begin), increment(increment) {}

            CounterIterator &operator++() noexcept {
                counter += increment;
                return *this;
            }

            constexpr bool operator==(const CounterIterator &other) const noexcept {
                return counter == other.counter;
            }

            constexpr bool operator==(Unreachable) const noexcept {
                return false;
            }

            constexpr bool operator!=(const CounterIterator &other) const noexcept {
                return !(*this == other);
            }

            constexpr bool operator!=(Unreachable) const noexcept {
                return true;
            }

            constexpr T operator*() const noexcept {
                return counter;
            }

        private:
            T counter;
            T increment;
        };

        template<typename T = std::size_t>
        struct CounterContainer {
            explicit constexpr CounterContainer(T start, T increment) noexcept: start(start), increment(increment) {}

            [[nodiscard]] CounterIterator<T> begin() const noexcept {
                return CounterIterator<T>(start, increment);
            }

            [[nodiscard]] static constexpr Unreachable end() noexcept {
                return Unreachable{};
            }

        private:
            T start;
            T increment;
        };

        template<typename Container, typename Function>
        struct TransformContainer {
            template<typename T>
            TransformContainer(T &&init, Function &&func) : container(std::forward<T>(init)),
                                                            f(std::forward<Function>(func)) {}

            template<bool End = false>
            class TransformIterator {
                static_assert(std::is_nothrow_copy_constructible_v<Function>, "Function object must be copyable");
                using Iterator = std::conditional_t<End, decltype(std::end(
                        std::declval<Container>())), decltype(std::begin(std::declval<Container>()))>;
                using Element = decltype(*std::declval<Iterator>());
                static_assert(std::is_invocable_v<Function, Element>,
                              "Function object is not callable with container element type");
                using InvocationResult = std::invoke_result_t<Function, Element>;
                friend TransformIterator<!End>;
            public:
                TransformIterator(Container &container,
                                  const Function &func) noexcept(std::is_nothrow_copy_constructible_v<Function>): it(
                        create(container)), f(func) {}

                TransformIterator &operator++() noexcept(noexcept(++this->it)){
                    ++it;
                    return *this;
                }

                template<bool B>
                bool operator==(const TransformIterator<B> &other) const noexcept(noexcept(this->it == this->it)) {
                    return it == other.it;
                }

                template<bool B>
                bool operator!=(const TransformIterator<B> &other) const noexcept(noexcept(*this == *this)) {
                    return !(*this == other);
                }

                auto operator*() const noexcept(noexcept(*(this->it))) -> InvocationResult {
                    return f(*it);
                }

                template<bool ReturnsRef = std::is_lvalue_reference_v<InvocationResult>>
                auto operator->() const noexcept(noexcept(**this))
                    -> std::enable_if_t<ReturnsRef, std::add_pointer_t<std::remove_reference_t<InvocationResult>>> {
                    return &**this;
                }

            private:
                static auto create(Container &container) noexcept {
                    if constexpr(End) {
                        return std::end(container);
                    } else {
                        return std::begin(container);
                    }
                }

                Iterator it;
                Function f;
            };

            TransformIterator<false> begin() const noexcept(std::is_nothrow_constructible_v<TransformIterator<false>,
                    std::add_lvalue_reference<Container>, Function>) {
                return TransformIterator<false>(container, f);
            }

            TransformIterator<true> end() const noexcept(std::is_nothrow_constructible_v<TransformIterator<true>,
                    std::add_lvalue_reference<Container>, Function>) {
                return TransformIterator<true>(container, f);
            }

        private:
            Container container;
            Function f;
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
     * @tparam T type of enumerate counter (default std::size_t)
     * @param container Arbitrary number of containers
     * @param start Optional index offset (default 0)
     * @param increment Optional index increment (default 1)
     * @return zip-container class that provides begin and end members to be used in range based for-loops.
     */
    template<typename Container, typename T = std::size_t>
    auto enumerate(Container &&container, T start = T(0), T increment = T(1)) {
        return zip(impl::CounterContainer(start, increment), std::forward<Container>(container));
    }

    /**
     * enumerate variant that does not allow manipulation of the container elements
     * @tparam Container Container type that supports iteration
     * @tparam T type of enumerate counter (default std::size_t)
     * @param container Arbitrary number of containers
     * @param start Optional index offset (default 0)
     * @param increment Optional index increment (default 1)
     * @return zip-container class that provides begin and end members to be used in range based for-loops.
     */
    template<typename Container, typename T = std::size_t>
    auto const_enumerate(Container &&container, T start = T(0), T increment = T(1)) {
        return const_zip(impl::CounterContainer(start, increment), std::forward<Container>(container));
    }

    template<typename Container, typename Function>
    auto transform(Container &&container, Function &&function) {
        return impl::TransformContainer<Container, Function>(std::forward<Container>(container),
                                                             std::forward<Function>(function));
    }
}

#endif //ITERATORTOOLS_ITERATORS_HPP
