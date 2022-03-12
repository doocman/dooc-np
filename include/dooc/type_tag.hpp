//          Copyright Robin Söderholm 2021 - 2022.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)

#ifndef DOOC_WIP_HRP_AND_AP_PUBLIC_INCLUDE_DOOC_TYPE_TAG_HPP
#define DOOC_WIP_HRP_AND_AP_PUBLIC_INCLUDE_DOOC_TYPE_TAG_HPP

#include <array>
#include <iterator>
#include <string_view>

namespace dooc {
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

template <template_string> struct template_tag {};

template <typename T, std::size_t tN>
  requires std::is_convertible_v<T, std::string_view>
constexpr bool operator==(T const &lhs, template_string<tN> const &rhs) {
  return static_cast<std::string_view>(lhs) ==
         static_cast<std::string_view>(rhs);
}

template <template_string t1, template_string t2>
constexpr bool operator==(template_tag<t1> const &, template_tag<t2> const &) {
  return t1 == t2;
}

template <template_string... tTags> struct template_string_list_t {
public:
  static constexpr std::size_t size() noexcept { return sizeof...(tTags); }
};

template <typename> constexpr bool is_template_string_list = false;

template <template_string... tTags>
constexpr bool is_template_string_list<template_string_list_t<tTags...>> = true;

template <typename T>
concept template_string_list_c =
    is_template_string_list<std::remove_cvref_t<T>>;

} // namespace dooc

#endif
