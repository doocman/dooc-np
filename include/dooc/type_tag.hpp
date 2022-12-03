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
  std::array<char, tN - 1> data_;
  using size_type = std::size_t;

  using string_t = char[tN];

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

  constexpr operator std::string_view() const noexcept {
    return {data_.data(), data_.size()};
  }
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
  if constexpr(sizeof...(tTags) > 0) {
    return static_cast<std::size_t>(
      static_cast<std::ptrdiff_t>(find_string<tTags...>(str)) + 1);
  } else {
    return 1;
  }
}

template <template_string... tTags>
constexpr std::size_t find_string(std::string_view str, template_string_list_t<tTags...>) {
  return find_string<tTags...>(str);
}

} // namespace dooc

#endif
