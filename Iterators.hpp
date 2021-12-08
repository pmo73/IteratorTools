/**
 * @author tim Luchterhand
 * @date 10.09.21
 * @brief This file contains the definitions of Python-like zip- and enumerate-functions. They can be used in range
 * based for-loops to loop over multiple ranges at the same time, or to index a range while looping respectively.
 */

#ifndef ITERATORTOOLS_ITERATORS_HPP
#define ITERATORTOOLS_ITERATORS_HPP

#include <tuple>

#define NOEXCEPT(OP, NAME) \
        template<typename T> \
        struct NAME { \
            static constexpr bool value = false; \
        }; \
        template<typename ...Ts> \
        struct NAME <std::tuple<Ts...>> { \
            static constexpr bool value = (... && noexcept(OP std::declval<std::add_lvalue_reference_t<Ts>>())); \
        };                 \
        template<typename T> \
        inline constexpr bool NAME##_v = NAME<T>::value;

namespace iterators {
    namespace impl {
        template<bool Cond, typename T>
        using reference_if_t = std::conditional_t<Cond, std::add_lvalue_reference_t<T>, T>;

        template<bool Cond, typename T>
        using const_if_t = std::conditional_t<Cond, std::add_const_t<T>, T>;

        template<typename T>
        std::true_type
        container_test(decltype(std::begin(std::declval<T>()), std::end(std::declval<T>()), std::declval<T>()));

        template<typename T>
        std::false_type container_test(...);

        template<typename T>
        struct is_container : decltype(container_test<T>(std::declval<T>())) {};

        template<typename T>
        constexpr inline bool is_container_v = is_container<T>::value;

        template<typename T>
        std::true_type deref_test(decltype(*std::declval<T>(), std::declval<T>()));

        template<typename T>
        std::false_type deref_test(...);

        template<typename T>
        struct dereferencible : decltype(deref_test<T>(std::declval<T>())) {};

        template<typename T>
        constexpr inline bool dereferencible_v = dereferencible<T>::value;

        struct empty {};

        template<typename T, bool B>
        struct dereference {
            using type = empty;
        };

        template<typename T>
        struct dereference<T, true> {
            using type = decltype(*std::declval<T>());
        };

        template<typename T>
        using dereference_t = typename dereference<T, dereferencible_v<T>>::type;

        template<typename T>
        struct values{};

        template<typename ...Ts>
        struct values<std::tuple<Ts...>> {
            using type = std::tuple<dereference_t<Ts>...>;
        };

        template<typename T>
        using values_t = typename values<T>::type;

        NOEXCEPT(++, is_nothrow_incrementible)

        NOEXCEPT(*, is_nothrow_dereferencible)

        template<typename Iterators>
        class ZipIterator {
            using ValueTuple = impl::values_t<Iterators>;
        public:
            explicit ZipIterator(const Iterators &iterators) : iterators(iterators) {}

            template<typename ...Its>
            explicit ZipIterator(Its ...its) : iterators(std::make_tuple(its...)) {}

            ZipIterator &operator++() noexcept(impl::is_nothrow_incrementible_v<Iterators>) {
                std::apply([](auto &&...it) { (++it, ...); }, iterators);
                return *this;
            }

            template<typename Its>
            constexpr bool operator==(const ZipIterator<Its> &other) const
            noexcept(noexcept(this->oneEqual(this->iterators, other.getIterators()))) {
                return oneEqual(iterators, other.getIterators());
            }

            template<typename Its>
            constexpr bool operator!=(const ZipIterator<Its> &other) const noexcept(noexcept(*this == other)) {
                return !(*this == other);
            }

            auto operator*() const noexcept(impl::is_nothrow_dereferencible_v<Iterators>) {
                return std::apply([](auto &&...it) { return ValueTuple(*it...); }, iterators);
            }

            constexpr auto getIterators() const noexcept -> const Iterators& {
                return iterators;
            }

        private:
            template<typename Tuple1, typename Tuple2, std::size_t ...Idx>
            static constexpr bool
            oneEqualImpl(const Tuple1 &t1, const Tuple2 &t2, std::index_sequence<Idx...>) noexcept((noexcept(
                    std::get<Idx>(t1) == std::get<Idx>(t2)) && ...)) {
                return (... || (std::get<Idx>(t1) == std::get<Idx>(t2)));
            }

            template<typename Tuple1, typename Tuple2>
            static constexpr bool oneEqual(const Tuple1 &t1, const Tuple2 &t2)
            noexcept(noexcept(oneEqualImpl(t1, t2, std::make_index_sequence<std::tuple_size_v<Tuple1>>{}))) {
                static_assert(std::tuple_size_v<Tuple1> == std::tuple_size_v<Tuple2>);
                return oneEqualImpl(t1, t2, std::make_index_sequence<std::tuple_size_v<Tuple1>>{});
            }

            Iterators iterators;
        };

        template<typename ...Iterable>
        struct ZipContainer {
        private:
            using ContainerTuple = std::tuple<Iterable...>;
            using IteratorTuple = std::tuple<decltype(std::begin(
                    std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
            using IteratorSentinelTuple = std::tuple<decltype(std::end(
                    std::declval<std::add_lvalue_reference_t<Iterable>>()))...>;
        public:
            template<typename ...Container>
            explicit ZipContainer(Container &&...containers) : containers(std::forward<Container>(containers)...) {}


            ZipIterator<IteratorTuple> begin() {
                return ZipIterator<IteratorTuple>(
                        std::apply([](auto &&...c) { return std::tuple(std::begin(c)...); }, containers));
            }

            ZipIterator<IteratorSentinelTuple> end() {
                return ZipIterator<IteratorSentinelTuple>(
                        std::apply([](auto &&...c) { return std::tuple(std::end(c)...); }, containers));
            }

            ZipIterator<IteratorTuple> begin() const {
                return ZipIterator<IteratorTuple>(
                        std::apply([](auto &&...c) { return std::tuple(std::begin(c)...); }, containers));
            }

            ZipIterator<IteratorSentinelTuple> end() const {
                return ZipIterator<IteratorSentinelTuple>(
                        std::apply([](auto &&...c) { return std::tuple(std::end(c)...); }, containers));
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


        template<typename Iterator, typename Function>
        class TransformIterator {
            static_assert(std::is_copy_constructible_v<Function>, "Function object must be copyable");
            using Element = decltype(*std::declval<Iterator>());
            static_assert(std::is_invocable_v<Function, Element>,
                          "Function object is not callable with container element type");
            using InvocationResult = std::invoke_result_t<Function, Element>;
        public:
            TransformIterator(const Iterator &iterator, const Function &func) noexcept(
            std::is_nothrow_copy_constructible_v<Iterator> && std::is_nothrow_copy_constructible_v<Function>): it(
                    iterator), f(func) {}

            TransformIterator &operator++() noexcept(noexcept(++this->it)) {
                ++it;
                return *this;
            }

            bool operator==(const TransformIterator &other) const noexcept(noexcept(this->it == this->it)) {
                return it == other.it;
            }

            bool operator!=(const TransformIterator &other) const noexcept(noexcept(*this == *this)) {
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
            Iterator it;
            Function f;
        };
        template<typename Container, typename Function>
        struct TransformContainer {
            using ContainerType = std::remove_reference_t<Container>;
            using It = decltype(std::begin(std::declval<std::add_lvalue_reference_t<ContainerType>>()));
            using ConstIt = decltype(std::begin(
                    std::declval<std::add_lvalue_reference_t<std::add_const_t<ContainerType>>>()));
            using Sentinel = decltype(std::end(std::declval<std::add_lvalue_reference_t<ContainerType>>()));
            using ConstSentinel = decltype(std::end(
                    std::declval<std::add_lvalue_reference_t<std::add_const_t<ContainerType>>>()));

            template<typename T>
            TransformContainer(T &&init, Function &&func) : container(std::forward<T>(init)),
                                                            f(std::forward<Function>(func)) {}

            auto begin() const noexcept(std::is_nothrow_constructible_v<TransformIterator<ConstIt, Function>,
                    ConstIt, Function>) {
                return TransformIterator<ConstIt, Function>(std::begin(container), f);
            }

            auto end() const noexcept(std::is_nothrow_constructible_v<TransformIterator<ConstSentinel, Function>,
                    ConstSentinel, Function>) {
                return TransformIterator<ConstSentinel, Function>(std::end(container), f);
            }

            auto begin() noexcept(std::is_nothrow_constructible_v<TransformIterator<It, Function>, It, Function>) {
                return TransformIterator<It, Function>(std::begin(container), f);
            }

            auto end() noexcept(std::is_nothrow_constructible_v<TransformIterator<Sentinel, Function>,
                    Sentinel, Function>) {
                return TransformIterator<Sentinel, Function>(std::end(container), f);
            }
        private:
            Container container;
            Function f;
        };
    }

    /**
     * Function that is used to create a ZipIterator from an arbitrary number of iterators
     * @tparam Iterators type of iterators
     * @param iterators arbitrary number of iterators
     * @return ZipIterator
     */
    template<typename ...Iterators, std::enable_if_t<(impl::dereferencible_v<Iterators> && ...), int> = 0>
    auto zip(Iterators ...iterators) -> impl::ZipIterator<std::tuple<Iterators...>> {
        return impl::ZipIterator<std::tuple<Iterators...>>(iterators...);
    }

    /**
     * Function that can be used in range based loops to emulate the zip iterator from python.
     * As in python: if the passed containers have different lengths, the container with the least items decides
     * the overall range
     * @tparam Iterable Container types that support iteration
     * @param iterable Arbitrary number of containers
     * @return zip-container class that provides begin and end members to be used in range based for-loops
     */
    template<typename ...Iterable, std::enable_if_t<(impl::is_container_v<Iterable> && ...), int> = 0>
    auto zip(Iterable &&...iterable) {
        return impl::ZipContainer<Iterable...>(std::forward<Iterable>(iterable)...);
    }

    /**
     * Zip variant that does not allow manipulation of the container elements
     * @tparam Iterable Container types that support iteration
     * @param iterable Arbitrary number of containers
     * @return zip-container class that provides begin and end members to be used in range based for-loops.
     */
    template<typename ...Iterable>
    auto const_zip(Iterable &&...iterable) {
        return impl::ZipContainer<impl::reference_if_t<std::is_lvalue_reference_v<Iterable>,
                std::add_const_t<std::remove_reference_t<Iterable>>>...>(std::forward<Iterable>(iterable)...);
    }

    /**
     * Function that can be used in range based loops to emulate the enumerate iterator from python.
     * @tparam Container Container type that supports iteration
     * @tparam T type of enumerate counter (default std::size_t)
     * @param container Source container
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
     * @param container Source container
     * @param start Optional index offset (default 0)
     * @param increment Optional index increment (default 1)
     * @return zip-container class that provides begin and end members to be used in range based for-loops.
     */
    template<typename Container, typename T = std::size_t>
    auto const_enumerate(Container &&container, T start = T(0), T increment = T(1)) {
        return const_zip(impl::CounterContainer(start, increment), std::forward<Container>(container));
    }

    /**
     * Creates a TransformIterator from an arbitrary iterator and a function object
     * @tparam Iterator type of iterator
     * @tparam Function  type of function
     * @param iterator base iterator
     * @param function transformation function
     * @return TransformIterator
     */
    template<typename Iterator, typename Function, std::enable_if_t<impl::dereferencible_v<Iterator>, int> = 0>
    auto transform(const Iterator &iterator, Function &&function) -> impl::TransformIterator<Iterator, Function> {
        return impl::TransformIterator<Iterator, Function>(iterator, std::forward<Function>(function));
    }

    /**
     * Transform view similar to std::ranges::transform_view
     * @tparam Container Container type that supports iteration
     * @tparam Function Function object that is callable with container elements
     * @param container source container
     * @param function function object that is applied to each element
     * @return TransformContainer class that provides begin and end members to be used in range based for loops
     */
    template<typename Container, typename Function, std::enable_if_t<impl::is_container_v<Container>, int> = 0>
    auto transform(Container &&container, Function &&function) {
        return impl::TransformContainer<Container, Function>(std::forward<Container>(container),
                                                             std::forward<Function>(function));
    }
}

#endif //ITERATORTOOLS_ITERATORS_HPP
