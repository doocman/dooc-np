//          Copyright Robin Söderholm 2021 - 2022.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)

#ifndef DOOC_NP_DOOC_TYPE_TAG_HPP
#define DOOC_NP_DOOC_TYPE_TAG_HPP

#include <array>
#include <iterator>
#include <string_view>

namespace dooc {
namespace details {
template <typename... Ts>
constexpr inline void dooc_np_unused(Ts const &...) noexcept {}
} // namespace details
template <std::size_t tN> struct template_string {
private:
  using data_t = std::array<char, tN - 1>;

public:
  data_t data_;
  using size_type = std::size_t;

  using string_t = char[tN];
  using iterator = typename data_t::iterator;
  using const_iterator = typename data_t::const_iterator;

  template_string() = default;

  explicit(false) constexpr template_string(
      string_t const &string_in) noexcept {
    using std::begin;
    std::copy_n(begin(string_in), data_.size(), begin(data_));
  }
  explicit(false) constexpr template_string(
      std::array<char const, tN> const &string_in) noexcept {
    using std::begin;
    std::copy_n(begin(string_in), data_.size(), begin(data_));
  }
  explicit constexpr template_string(
      std::ranges::input_range auto const &string_in) noexcept {
    using std::begin;
    std::copy_n(begin(string_in), data_.size(), begin(data_));
  }
  template <std::input_iterator TIt, std::sentinel_for<TIt> TSent>
  constexpr template_string(TIt beg, TSent end) noexcept {
    std::copy(beg, end, std::begin(data_));
  }

  constexpr operator std::string_view() const noexcept {
    return {data_.data(), data_.size()};
  }
  constexpr iterator begin() noexcept { return std::begin(data_); }
  constexpr const_iterator cbegin() const noexcept {
    return std::cbegin(data_);
  }
  constexpr const_iterator begin() const noexcept { return cbegin(); }
  constexpr iterator end() noexcept { return std::end(data_); }
  constexpr const_iterator cend() const noexcept { return std::cend(data_); }
  constexpr const_iterator end() const noexcept { return cend(); }
};

template <std::size_t tN>
constexpr std::size_t size(template_string<tN> const &) noexcept {
  return tN;
}

template <typename T, std::size_t tN>
  requires std::is_convertible_v<T, std::string_view>
constexpr bool operator==(T const &lhs, template_string<tN> const &rhs) {
  return static_cast<std::string_view>(lhs) ==
         static_cast<std::string_view>(rhs);
}

namespace details {
template <std::size_t tN, std::size_t... tNs>
constexpr auto concat_to_it(auto dest, template_string<tN> const &c,
                            template_string<tNs> const &...rest) {
  using std::begin, std::end;
  dest = std::copy_n(begin(c), static_cast<std::ptrdiff_t>(tN - 1), dest);
  if constexpr (sizeof...(tNs) > 0) {
    return concat_to_it(dest, rest...);
  } else {
    return dest;
  }
}
} // namespace details
template <std::size_t... tNs,
          std::size_t tFullSize = (tNs + ...) + 1 - sizeof...(tNs)>
constexpr template_string<tFullSize>
concat(template_string<tNs> const &...str) {
  using std::begin;
  template_string<tFullSize> data;
  details::concat_to_it(begin(data), str...);
  return template_string<tFullSize>(data);
}

static_assert(concat(template_string("ab"), template_string("cb")) ==
              template_string("abcb"));

template <template_string... tTags> struct template_string_list_t {
  static constexpr std::size_t size() noexcept { return sizeof...(tTags); }
};

template <template_string... tTags1, template_string... tTags2>
constexpr template_string_list_t<tTags1..., tTags2...>
operator+(template_string_list_t<tTags1...>,
          template_string_list_t<tTags2...>) noexcept {
  return {};
}

template <typename> constexpr bool is_template_string_list = false;

template <template_string... tTags>
constexpr bool is_template_string_list<template_string_list_t<tTags...>> = true;

template <typename T>
concept template_string_list_c =
    is_template_string_list<std::remove_cvref_t<T>>;

constexpr auto
combine_string_lists(template_string_list_c auto const &...lists) noexcept {
  return (lists + ...);
}

template <template_string tTag, template_string... tTags>
constexpr std::size_t find_string(std::string_view str) {
  if (str == tTag) {
    return 0;
  }
  if constexpr (sizeof...(tTags) > 0) {
    return static_cast<std::size_t>(
        static_cast<std::ptrdiff_t>(find_string<tTags...>(str)) + 1);
  } else {
    return 1;
  }
}

template <template_string... tTags>
constexpr std::size_t find_string(std::string_view str,
                                  template_string_list_t<tTags...>) {
  return find_string<tTags...>(str);
}

} // namespace dooc

#endif
