#pragma once

#include <array>
#include <memory>
#include <algorithm>

namespace sunstone_rtls {
    namespace detail {
        template<typename T, std::size_t Capacity>
        class static_ring_iterator;
    }

    template<typename T, std::size_t Capacity>
    class static_ring_vector {
    public:
        typedef T value_type;
        typedef T *pointer;
        typedef T &reference;
        typedef T const &const_reference;
        typedef T const *const_pointer;

        typedef std::size_t size_type;

        typedef detail::static_ring_iterator<T      , Capacity> iterator;
        typedef detail::static_ring_iterator<const T, Capacity> const_iterator;

        typedef std::reverse_iterator<iterator      > reverse_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

        // construct + assign
        static_ring_vector() noexcept : m_begin(begin_capacity()), m_size{} {};

        template<typename InputIt>
        static_ring_vector(InputIt begin, InputIt end)
            : static_ring_vector() {
            assign(begin, end);
        }

        explicit static_ring_vector(size_type count) noexcept(noexcept(T()))
                : m_begin(begin_capacity()), m_size{std::min(count, Capacity)}  {
            std::uninitialized_default_construct_n(m_begin, m_size);
        }

        explicit static_ring_vector(size_type count, const_reference value) noexcept(noexcept(T(value)))
            : m_begin(begin_capacity()), m_size{std::min(count, Capacity)}  {
            std::uninitialized_fill_n(m_begin, m_size, value);
        }

        template<typename U, std::size_t UCapacity>
        static_ring_vector(const static_ring_vector<U, UCapacity>& other) noexcept(noexcept(T(*other.begin())))
            : static_ring_vector(other.begin(), other.end()) {}

        template<typename U, std::size_t UCapacity>
        static_ring_vector(static_ring_vector<U, UCapacity>&& other) noexcept(noexcept(T(std::move(*other.begin()))))
            : static_ring_vector(std::make_move_iterator(other.begin()), std::make_move_iterator(other.end())) {}

        static_ring_vector(std::initializer_list<T> init) noexcept(noexcept(T(std::move(*init.begin()))))
            : static_ring_vector(std::make_move_iterator(init.begin()), std::make_move_iterator(init.end())) {}

        template<typename U, std::size_t UCapacity>
        static_ring_vector& operator=(const static_ring_vector<U, UCapacity>& other) {
            assign(other.begin(), other.end());
            return *this;
        }

        template<typename U, std::size_t UCapacity>
        static_ring_vector& operator=(static_ring_vector<U, UCapacity>&& other) {
            assign(std::make_move_iterator(other.begin()), std::make_move_iterator(other.end()));
            return *this;
        }

        ~static_ring_vector() {
            clear();
        }

        void assign(size_type count, const_reference value) {
            if (count <= size())
                erase(std::fill_n(begin(), count, value), end());
            else
                std::uninitialized_fill_n(std::fill_n(begin(), size(), value), std::min(Capacity, count) - size(), value),
                    m_size = count;
        }

        template<class InputIt>
        void assign(InputIt first, InputIt last) {
            assign_impl(first, last, typename std::iterator_traits<InputIt>::iterator_category{});
        }

        void assign(std::initializer_list<T> iList) {
            assign(std::make_move_iterator(iList.begin()), std::make_move_iterator(iList.end()));
        }

        // element access
        [[nodiscard]] reference at(size_type n) {
            if (n >= m_size)
                throw std::out_of_range("Static ring vector overindexed. Size: " + std::to_string(m_size) + ", but requested: " + std::to_string(n) + ".th element. Capacity: " + std::to_string(Capacity));
            return (*this)[n];
        }

        [[nodiscard]] const_reference at(size_type n) const {
            if (n >= m_size)
                throw std::out_of_range("Static ring vector overindexed. Size: " + std::to_string(m_size) + ", but requested: " + std::to_string(n) + ".th element. Capacity: " + std::to_string(Capacity));
            return (*this)[n];
        }

        [[nodiscard]] reference operator[](size_type n) {
            return *iterator{static_cast<std::ptrdiff_t>(n), m_begin, end_capacity()};
        }

        [[nodiscard]] const_reference operator[](size_type n) const {
            return *const_iterator{static_cast<std::ptrdiff_t>(n), m_begin, end_capacity()};
        }

        [[nodiscard]] reference front() {
            return *begin();
        }

        [[nodiscard]] const_reference front() const {
            return *begin();
        }

        [[nodiscard]] reference back() {
            return *std::prev(end());
        }

        [[nodiscard]] const_reference back() const {
            return *std::prev(end());
        }

        [[nodiscard]] pointer data() noexcept {
            return begin_capacity();
        }

        [[nodiscard]] const_pointer data() const noexcept {
            return begin_capacity();
        }

        // iterators
        [[nodiscard]] iterator begin() {
            return iterator{0, m_begin, end_capacity()};
        }

        [[nodiscard]] const_iterator begin() const {
            return const_iterator{0, m_begin, end_capacity()};
        }

        [[nodiscard]] iterator end() {
            return iterator{static_cast<std::ptrdiff_t>(m_size), m_begin, end_capacity()};
        }

        [[nodiscard]] const_iterator end() const {
            return const_iterator{static_cast<std::ptrdiff_t>(m_size), m_begin, end_capacity()};
        }

        [[nodiscard]] const_iterator cbegin() const {
            return begin();
        }

        [[nodiscard]] const_iterator cend() const {
            return end();
        }

        [[nodiscard]] reverse_iterator rbegin() {
            return reverse_iterator{end()};
        }

        [[nodiscard]] const_reverse_iterator rbegin() const {
            return const_reverse_iterator{end()};
        }

        [[nodiscard]] reverse_iterator rend() {
            return reverse_iterator{begin()};
        }

        [[nodiscard]] const_reverse_iterator rend() const {
            return const_reverse_iterator{begin()};
        }

        [[nodiscard]] const_reverse_iterator crbegin() const {
            return rbegin();
        }

        [[nodiscard]] const_reverse_iterator crend() const {
            return rend();
        }

        // capacity
        [[nodiscard]] size_type size() const {
            return m_size;
        }

        [[nodiscard]] bool empty() const {
            return m_size == 0;
        }

        [[nodiscard]] bool full() const {
            return m_size == Capacity;
        }

        [[nodiscard]] size_type capacity() const {
            return Capacity;
        }

        [[nodiscard]] size_type max_size() const {
            return Capacity;
        }

        // modifiers
        void clear() {
            std::destroy_n(begin(), m_size);
            m_size = 0;
        }

        iterator insert(const_iterator pos, const_reference value) {
            return insert_one(make_non_const(pos), value);
        }

        iterator insert(const_iterator pos, T&& value) {
            return insert_one(make_non_const(pos), std::move(value));
        }

        void insert(const_iterator pos, size_type count, const_reference value) {
            struct It : std::iterator_traits<const_pointer> {
                const_pointer value;
                size_type count;
                const_reference operator*() {
                    return *value;
                }
                It& operator++() {
                    ++count;
                    return *this;
                }

                It operator++(int) {
                    It cp(*this);
                    ++*this;
                    return cp;
                }
                std::ptrdiff_t operator-(const It& it) const {
                    return count - it.count;
                }

                It& operator+=(std::ptrdiff_t n) {
                    count += n;
                    return *this;
                }

                bool operator!=(const It& it) const {
                    return count != it.count;
                }

                bool operator==(const It& it) const {
                    return count == it.count;
                }
            };

            insert(pos, It{{}, std::addressof(value), size_type{}}, It{{}, std::addressof(value), count});
        }

        template<class InputIt>
        void insert(const_iterator pos, InputIt first, InputIt last) {
            insert_impl(make_non_const(pos), first, last, typename std::iterator_traits<InputIt>::iterator_category{});
        }

        void insert(const_iterator pos, std::initializer_list<T> iList) {
            insert(pos, std::make_move_iterator(iList.begin()), std::make_move_iterator(iList.end()));
        }

        template<typename... Args>
        iterator emplace(const_iterator pos, Args&&... args) {
            auto toPos = pos.index();
            if (Capacity == m_size) {
                if (!toPos) return end(); // skip emplace
                pop_front(), --toPos;
            }

            if (toPos >= (m_size + 1) / 2) {
                for (iterator last = end(), d_last = std::next(last); toPos != last.index();) {
                    std::uninitialized_move_n(--d_last, 1, --last);
                    std::destroy_at(last.ptr());
                }
            } else {
                for (iterator d_first = std::prev(capacity_end_it()), first = begin(); first.index() != toPos; ++d_first, ++first) {
                    std::uninitialized_move_n(d_first, 1, first);
                    std::destroy_at(first.ptr());
                }
                decrement_begin();
            }
            ++m_size;

            iterator elem{toPos, m_begin, end_capacity()};
            new (elem.ptr()) T(std::forward<Args>(args)...);
            return elem;
        }

        iterator erase(const_iterator pos) {
            return erase(pos, std::next(pos));
        }

        iterator erase(const_iterator cfrom, const_iterator cto) {
            iterator from = make_non_const(cfrom), to = make_non_const(cto);
            if (const auto countToEnd = m_size - to.index(), countFromBegin = from.index(); !countToEnd || !countFromBegin) {
                std::destroy(from, to);
                if (countToEnd)
                    m_begin = to.ptr();
                m_size = countFromBegin + countToEnd;
                return iterator{countFromBegin, m_begin, end_capacity()};
            } else if (countFromBegin >= countToEnd) {
                return erase(std::move(to, end(), from), end()) - countToEnd;
            } else {
                return erase(begin(), std::move_backward(begin(), from, to)) + countFromBegin;
            }
        }

        void push_back(const T& elem) {
            if (m_size == Capacity) {
                *m_begin = elem;
                increment_begin();
            } else {
                std::uninitialized_copy_n(std::addressof(elem), 1, end());
                ++m_size;
            }
        }

        void push_back(T&& elem) {
            if (m_size == Capacity) {
                *m_begin = std::move(elem);
                increment_begin();
            } else {
                std::uninitialized_move_n(std::addressof(elem), 1, end());
                ++m_size;
            }
        }

        template<typename ...Args>
        reference emplace_back(Args&&... args) {
            pointer emplace_at;
            if (m_size == Capacity) {
                std::destroy_at(m_begin);
                emplace_at = m_begin;
                increment_begin();
            } else {
                emplace_at = end().ptr();
                ++m_size;
            }
            return *new (emplace_at) T(std::forward<Args>(args)...);
        }

        void pop_back() {
            std::destroy_at(std::prev(end()).ptr());
            --m_size;
        }

        void push_front(const T& elem) {
            if (m_size != Capacity) {
                decrement_begin();
                ++m_size;
                new (m_begin) T(elem);
            }
        }

        void push_front(T&& elem) {
            if (m_size != Capacity) {
                decrement_begin();
                ++m_size;
                new (m_begin) T(std::move(elem));
            }
        }

        template<typename ...Args>
        iterator emplace_front(Args&&... args) {
            if (m_size != Capacity) {
                decrement_begin();
                ++m_size;
                new (m_begin) T(std::forward<Args>(args)...);
                return begin();
            }
            return end();
        }

        void pop_front() {
            std::destroy_at(m_begin);
            increment_begin();
            --m_size;
        }

        void swap(static_ring_vector& other) {
            static_ring_vector& less = other.size() < size() ? other : *this;
            static_ring_vector& greater = other.size() < size() ? *this : other;
            iterator eraseFrom;
            less.insert(less.end(),
                         std::make_move_iterator(eraseFrom = std::swap_ranges(less.begin(), less.end(), greater.begin())),
                         std::make_move_iterator(greater.end()));
            greater.erase(eraseFrom, greater.end());
            std::swap(m_size, other.m_size);
        }

    private:
        inline auto begin_capacity() const {
            return reinterpret_cast<const_pointer>(rawData.data());
        }

        inline auto begin_capacity() {
            return reinterpret_cast<pointer>(rawData.data());
        }

        inline auto end_capacity() const {
            return begin_capacity() + Capacity;
        }

        inline auto end_capacity() {
            return begin_capacity() + Capacity;
        }

        inline iterator capacity_end_it() {
            return iterator{static_cast<std::ptrdiff_t>(Capacity), m_begin, end_capacity()};
        }

        inline const_iterator capacity_end_it() const {
            return const_iterator{static_cast<std::ptrdiff_t>(Capacity), m_begin, end_capacity()};
        }

        inline void increment_begin() {
            if (++m_begin == end_capacity())
                m_begin -= Capacity;
        }

        inline void decrement_begin() {
            if (m_begin-- == begin_capacity())
                m_begin += Capacity;
        }

        inline iterator make_non_const(const_iterator it) {
            return iterator{it.index(), m_begin, end_capacity()};
        }

        template<typename X>
        inline iterator insert_one(iterator pos, X&& val) {
            auto toPos = pos.index();
            const bool full = Capacity == m_size;
            if (full) {
                if (!toPos) return end(); // skip insertion
                increment_begin(), --toPos, --m_size;
            }
            if (toPos >= (m_size + 1) / 2) {
                if (toPos != m_size) {
                    iterator last = end(), d_last = std::next(last);
                    if (!full)
                        std::uninitialized_move_n(--last, 1, --d_last);
                    if (toPos != last.index())
                        std::move_backward(begin() + toPos, last, d_last);
                }
            } else {
                if (toPos) {
                    iterator d_first = std::prev(capacity_end_it()), first = begin();
                    if (!full)
                        std::uninitialized_move_n(first++, 1, d_first++);
                    if (toPos != first.index())
                        std::move(first, begin() + toPos - 1, d_first);
                }
                decrement_begin();
            }

            iterator elem{toPos, m_begin, end_capacity()};
            if (!full && (toPos == m_size || !toPos)) {
                new (elem.ptr()) T(std::forward<X>(val));
            } else {
                *elem = std::forward<X>(val);
            }
            ++m_size;
            return elem;
        }

        template<typename FwIterator>
        inline void insert_impl(iterator pos, FwIterator first, FwIterator last, std::forward_iterator_tag) {
            if (first == last) return;

            auto toPos = pos.index();
            auto newElementsCount = std::distance(first, last);
            const auto elementsAtEnd = m_size - toPos;
            if (newElementsCount + elementsAtEnd > Capacity)
                std::advance(first, newElementsCount + elementsAtEnd - Capacity), newElementsCount = Capacity - elementsAtEnd;

            const auto elementsAtBegin = std::min<std::ptrdiff_t>(Capacity - elementsAtEnd - newElementsCount, toPos);
            const auto eraseElements = toPos - elementsAtBegin;

            //   0   eraseElements          toPos          m_size          Capacity
            //   |---------|------------------|---------------|---------------|
            //               elementsAtBegin    elementsAtEnd   uninitialized
            //        first               last
            //   [-----]|------------------|
            //            newElementsCount

            // -->
            //   |-----------------|------------------|---------------|
            //     elementsAtBegin   newElementsCount   elementsAtEnd

            auto copy = [](auto fromBegin, auto fromEnd, auto toBegin, auto toEnd, bool uninitialized) {
                for (;fromBegin != fromEnd && toBegin != toEnd; ++fromBegin, ++toBegin)
                    if (uninitialized)
                        new (std::addressof(*toBegin)) std::decay_t<decltype(*toBegin)>(*fromBegin);
                    else
                        *toBegin = *fromBegin;
                return std::pair{fromBegin, toBegin};
            };

            if (elementsAtEnd <= elementsAtBegin) {
                // toPos <- m_size
                auto fromBegin = std::make_reverse_iterator(std::make_move_iterator(end())),
                    fromEnd = std::make_reverse_iterator(std::make_move_iterator(pos));
                // eraseElements
                fromBegin = copy(fromBegin, fromEnd,
                               std::make_reverse_iterator(begin() + eraseElements),
                               std::make_reverse_iterator(begin()),
                               false).first;
                // uninitialized move
                fromBegin = copy(fromBegin, fromEnd,
                               std::make_reverse_iterator(end() + newElementsCount - eraseElements),
                               std::make_reverse_iterator(end()), true).first;
                // fill elementsAtEnd, rest
                std::copy(fromBegin, fromEnd, std::make_reverse_iterator(end()));

                // first -> last
                // elementsAtEnd
                first = copy(first, last, pos, end(), false).first;
                // uninitialized
                first = copy(first, last, end(), capacity_end_it(), true).first;
                // eraseElements
                std::copy(first, last, begin());

                m_begin = (begin() + eraseElements).ptr();
            } else {
                constexpr std::ptrdiff_t zero{};
                const std::ptrdiff_t startFrom = eraseElements - newElementsCount;
                const std::ptrdiff_t startFirst = toPos - newElementsCount;

                // eraseElements -> toPos
                auto fromBegin = begin() + eraseElements, fromEnd = pos;
                // uninitialized
                fromBegin = copy(fromBegin, fromEnd,
                                 capacity_end_it() - std::min(zero, startFrom),
                                 capacity_end_it(), true).first;
                // atBegin
                std::copy(fromBegin, fromEnd, begin());

                // first -> last
                // uninitialized
                first = copy(first, last,
                             capacity_end_it() - std::min(zero, startFirst),
                             capacity_end_it(), true).first;
                // atBegin
                std::copy(fromBegin, fromEnd, begin() + std::max(zero, startFirst));

                m_begin = (begin() + (startFrom < 0 ? Capacity - startFrom : startFrom)).ptr();
            }
            m_size = elementsAtBegin + newElementsCount + elementsAtEnd;
        }

        template<typename InputIt>
        inline void insert_impl(iterator pos, InputIt first, InputIt last, std::input_iterator_tag) {
            for (;first != last; ++first)
                pos = std::next(insert_one(pos, *first));
        }

        template<typename FwIterator>
        inline void assign_impl(FwIterator first, FwIterator last, std::forward_iterator_tag) {
            auto distance = std::distance(first, last);
            if (distance > Capacity)
                std::advance(first, distance - Capacity), distance = Capacity;

            if (distance <= m_size) {
                erase(std::copy_n(first, distance, begin()), end());
            } else {
                auto toBegin = begin(), toEnd = end();
                while (toBegin != toEnd)
                    *toBegin++ = *first++;
                std::uninitialized_copy(first, last, toBegin);
                m_size = distance;
            }
        }

        template<typename InputIt>
        inline void assign_impl(InputIt first, InputIt last, std::input_iterator_tag) {
            auto toCurr = begin(), toEnd = end();
            for (;toCurr != toEnd && first != last; ++toCurr, ++first)
                *toCurr = *first;

            if (first == last)
                erase(toCurr, toEnd);
            else {
                for (auto cap_end = capacity_end_it(); toCurr != cap_end && first != last; ++toCurr, ++first)
                    new (toCurr.ptr()) T(*first);
                m_size = toCurr.index();
            }

            for (;first != last; ++first)
                push_back(*first);
        }

        std::array<std::aligned_storage_t<sizeof(T), alignof(T)>, Capacity> rawData;
        pointer m_begin;
        size_type m_size{};
    };

    namespace detail {
        template<typename T, std::size_t Capacity>
        class static_ring_iterator : public std::iterator_traits<T*>, public std::tuple<typename std::iterator_traits<T*>::difference_type> {
        public:
            using difference_type = typename std::iterator_traits<T*>::difference_type;
            static_ring_iterator() = default;
            static_ring_iterator(const static_ring_iterator&) = default;
            static_ring_iterator(static_ring_iterator&&) noexcept = default;
            static_ring_iterator& operator=(const static_ring_iterator&) = default;
            static_ring_iterator& operator=(static_ring_iterator&&) noexcept = default;

            inline operator static_ring_iterator<const T, Capacity>() {
                return {index(), begin, cap_end};
            }

            [[nodiscard]] inline typename static_ring_iterator::reference operator*() const {
                return *ptr();
            }

            [[nodiscard]] inline typename static_ring_iterator::pointer operator->() const {
                return std::addressof(**this);
            }

            static_ring_iterator& operator++() {
                ++index();
                return *this;
            }

            static_ring_iterator operator++(int) {
                static_ring_iterator cp(*this);
                ++*this;
                return cp;
            }

            static_ring_iterator& operator--() {
                --index();
                return *this;
            }

            static_ring_iterator operator--(int) {
                static_ring_iterator cp(*this);
                --*this;
                return cp;
            }

            static_ring_iterator& operator+=(typename static_ring_iterator::difference_type n) {
                index() += n;
                return *this;
            }

            [[nodiscard]] friend static_ring_iterator operator+(static_ring_iterator it, typename static_ring_iterator::difference_type n) {
                return it += n;
            }

            [[nodiscard]] friend static_ring_iterator operator+(typename static_ring_iterator::difference_type n, static_ring_iterator it) {
                return it += n;
            }

            static_ring_iterator& operator-=(typename static_ring_iterator::difference_type n) {
                return *this += -n;
            }

            [[nodiscard]] friend static_ring_iterator operator-(static_ring_iterator it, typename static_ring_iterator::difference_type n) {
                return it -= n;
            }

            [[nodiscard]] inline typename static_ring_iterator::reference operator[](typename static_ring_iterator::difference_type n) const {
                return *(*this + n);
            }

            [[nodiscard]] friend difference_type operator-(const static_ring_iterator& lhs, const static_ring_iterator& rhs) {
                return lhs.index() - rhs.index();
            }

        private:
            static_ring_iterator(difference_type curr, T *begin, T *capEnd)
                    : std::tuple<difference_type>(curr), begin(begin), cap_end(capEnd) {}

            template<typename, std::size_t>
            friend class static_ring_vector;

            template<typename, std::size_t>
            friend class static_ring_iterator;

            inline difference_type& index() {
                return std::get<0>(*this);
            }

            inline difference_type index() const {
                return std::get<0>(*this);
            }

            inline typename static_ring_iterator::pointer ptr() const {
                auto ptr = begin + index();
                if (ptr >= cap_end)
                    ptr -= Capacity;
                return ptr;
            }

            T *begin, *cap_end;
        };
    }
}