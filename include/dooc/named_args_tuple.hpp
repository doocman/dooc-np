
//          Copyright Robin Söderholm 2021 - 2022.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)

#ifndef DOOC_NP_DOOC_NAMED_ARGS_TUPLE_HPP
#define DOOC_NP_DOOC_NAMED_ARGS_TUPLE_HPP

#include <algorithm>
#include <concepts>
#include <span>
#include <type_traits>

#include <dooc/type_tag.hpp>

namespace dooc {
template <typename T>
constexpr template_string_list_t contained_tags = T::arg_list_;

template <template_string, typename T> struct named_arg_t;
template <typename...> struct named_tuple;
template <typename> struct named_arg_properties;
template <typename T>
  requires(!std::is_same_v<T, std::remove_reference_t<T>>)
struct named_arg_properties<T> {
  static constexpr auto tag =
      named_arg_properties<std::remove_reference_t<T>>::tag;
  using type = typename named_arg_properties<std::remove_reference_t<T>>::type;
};

template <typename T> struct named_arg_properties<T const> {
  using type = std::add_const_t<typename named_arg_properties<T>::type>;
  static constexpr template_string tag = named_arg_properties<T>::tag;
};
template <typename T> struct named_arg_properties<T volatile> {
  using type = std::add_volatile_t<typename named_arg_properties<T>::type>;
  static constexpr template_string tag = named_arg_properties<T>::tag;
};
template <typename T> struct named_arg_properties<T const volatile> {
  using type = std::add_cv_t<typename named_arg_properties<T>::type>;
  static constexpr template_string tag = named_arg_properties<T>::tag;
};

template <typename T>
constexpr template_string named_arg_tag =
    named_arg_properties<std::remove_cvref_t<T>>::tag;

namespace details {
template <template_string>
consteval std::size_t index_of_template_string_helper(std::size_t current) {
  return current;
}
template <template_string to_find, template_string tString,
          template_string... tStrings>
consteval std::size_t index_of_template_string_helper(std::size_t current) {
  if constexpr (to_find == tString)
    return current;
  else
    return index_of_template_string_helper<to_find, tStrings...>(current + 1);
}

template <template_string to_find, template_string... tStrings>
consteval std::size_t index_of_template_string() {
  return index_of_template_string_helper<to_find, tStrings...>(0);
  // I'd love to just use this instead of recursion above, but gcc-10 doesn't
  // roll with it.
  //  constexpr std::array strings_arr = {
  //      static_cast<std::string_view>(tStrings)...};
  //  constexpr auto str_found =
  //      std::find(begin(strings_arr), end(strings_arr), to_find);
  //  return std::distance(begin(strings_arr), str_found);
}

template <template_string to_find, template_string... tStrings>
consteval std::size_t index_of_template_string_list(
    template_string_list_t<tStrings...> const &) noexcept {
  return index_of_template_string<to_find, tStrings...>();
}

template <typename, typename> struct is_tuple_convertible : std::false_type {};

template <typename T1, typename T2>
constexpr bool is_tuple_convertible_v = is_tuple_convertible<T1, T2>::value;
} // namespace details
template <template_string, typename T> struct named_tuple_element;
template <template_string tTag, typename T>
using named_tuple_element_t = typename named_tuple_element<tTag, T>::type;

template <template_string tTag, typename T>
struct named_tuple_element<tTag, const T> {
  using type = std::add_const_t<named_tuple_element_t<tTag, T>>;
};
template <template_string tTag, typename T>
struct named_tuple_element<tTag, volatile T> {
  using type = std::add_volatile_t<named_tuple_element_t<tTag, T>>;
};
template <template_string tTag, typename T>
struct named_tuple_element<tTag, const volatile T> {
  using type = std::add_cv_t<named_tuple_element_t<tTag, T>>;
};

template <typename T>
concept arg_with_any_name = requires(T) {
  named_arg_properties<std::remove_cvref_t<T>>::tag;
  typename named_arg_properties<std::remove_reference_t<T>>::type;
};

template <arg_with_any_name T, template_string tTag>
constexpr bool is_tagged_with =
    named_arg_properties<std::remove_cvref_t<T>>::tag == tTag;

template <typename T, template_string tTag>
concept arg_with_name = arg_with_any_name<T> && is_tagged_with<T, tTag>;

template <template_string tTag, arg_with_any_name... Ts>
constexpr bool at_least_one_is_tagged_with = (is_tagged_with<Ts, tTag> || ...);

template <arg_with_any_name Ts, template_string... tTag>
constexpr bool is_tagged_with_one_of = (is_tagged_with<Ts, tTag> || ...);

template <typename T, template_string... tTags>
concept contains_any_of_tags = (arg_with_name<T, tTags> || ...);

template <typename T, template_string tTag, typename T2>
concept tag_is_type = arg_with_name<T, tTag> && requires(T t) {
  { get<tTag>(t) } -> std::same_as<T2>;
};

template <typename T, template_string... tTags>
concept named_tuple_like = (arg_with_name<T, tTags> && ...) && requires {
  std::tuple_size<std::remove_cvref_t<T>>::value;
};

template <typename T>
constexpr std::size_t tuple_size_v = std::tuple_size<T>::value;

namespace details {

template <template_string tTag, arg_with_any_name... Ts>
constexpr auto covers_args_impl(std::tuple<Ts...>) noexcept
    -> std::conditional_t<(is_tagged_with<Ts, tTag> || ...), std::true_type,
                          std::false_type> {
  return {};
}

template <template_string... tTags, arg_with_any_name... Ts>
constexpr auto covers_args_impl(Ts &&...) noexcept {
  using t_help = std::tuple<Ts &&...>;
  if constexpr ((decltype(covers_args_impl<tTags>(
                    std::declval<t_help>()))::value &&...))
    return std::true_type{};
  else
    return std::false_type{};
}

template <template_string... tTags, arg_with_any_name... Ts>
constexpr auto covers_args_impl(template_string_list_t<tTags...>,
                                Ts &&...) noexcept
    -> decltype(covers_args_impl<tTags...>(std::declval<Ts>()...)) {
  return {};
}

template <template_string... tTags, arg_with_any_name... Ts>
constexpr auto are_args_impl(Ts &&...) noexcept {
  if constexpr (sizeof...(tTags) == sizeof...(Ts))
    return decltype(covers_args_impl<tTags...>(std::declval<Ts>()...)){};
  else
    return std::false_type{};
}
template <template_string... tTags, arg_with_any_name... Ts>
constexpr auto are_args_impl(template_string_list_t<tTags...>,
                             Ts &&...ts1) noexcept {
  return are_args_impl<tTags...>(ts1...);
}
} // namespace details

template <template_string_list_c tTags, arg_with_any_name... Ts>
constexpr bool covers_args = decltype(details::covers_args_impl(
    std::declval<tTags>(), std::declval<Ts>()...))::value;

template <template_string_list_c tTags, arg_with_any_name... Ts>
constexpr bool are_exactly_args = decltype(details::are_args_impl(
    std::declval<tTags>(), std::declval<Ts>()...))::value;

template <typename TArgList, arg_with_any_name... Ts>
constexpr bool args_fullfill = TArgList::template fullfilled_by<Ts...>;

template <template_string tTag, typename T> struct named_arg_t {
  using type = T;
  // static constexpr template_string tag = tTag;
  T value_;
  constexpr named_arg_t() = default;
  explicit constexpr named_arg_t(T const &v) : value_(v) {}
  template <typename... Us>
  explicit constexpr named_arg_t(Us &&...ctor_args)
      : value_(std::forward<Us>(ctor_args)...) {}

  constexpr named_arg_t(named_arg_t const &) = default;
  constexpr named_arg_t(named_arg_t &&) noexcept = default;
  constexpr named_arg_t &operator=(named_arg_t const &) = default;
  constexpr named_arg_t &operator=(named_arg_t &&) noexcept = default;

  operator T &() &noexcept { return value_; }
  operator T &&() &&noexcept {
    return static_cast<T&&>(value_);
  }
  operator T const &() const &noexcept  {
    return value_;
  }

  constexpr std::add_lvalue_reference_t<T> value() &noexcept { return value_; }
  constexpr std::conditional_t<std::is_reference_v<T>, T, T &&>
  value() &&noexcept {
    return std::forward<T>(value_);
  }
  constexpr std::conditional_t<std::is_reference_v<T>, T, T const &>
  value() const &noexcept {
    return value_;
  }

  static constexpr template_string<size(tTag)> tag() noexcept { return tTag; }
};

template <template_string tTag, typename T>
struct named_arg_properties<named_arg_t<tTag, T>> {
  constexpr static template_string tag = tTag;
  using type = T;
};

template <template_string tTag, arg_with_name<tTag> T>
constexpr decltype(auto) get(T &&t) {
  return std::forward<T>(t).value();
}

template <typename T, template_string tTag> struct named_type {
  template <typename T2>
    requires requires(T2) {
      typename named_arg_properties<std::remove_cvref_t<T2>>::type;
      { named_arg_properties<T2>::tag } -> std::equality_comparable;
    }
  static constexpr bool is_equivalent =
      named_arg_properties<T2>::tag == tTag &&
      std::is_convertible_v<typename named_arg_properties<T2>::type, T>;
  static constexpr bool optional = false;
};
template <template_string tTag> struct named_auto {
  template <typename T2>
    requires requires(T2) {
      typename named_arg_properties<T2>::type;
      { named_arg_properties<T2>::tag } -> std::equality_comparable;
    }
  static constexpr bool is_equivalent = named_arg_properties<T2>::tag == tTag;
  static constexpr bool optional = false;
};
template <typename T, template_string tTag> struct optional_typed_arg {
  template <typename T2>
  static constexpr bool is_equivalent =
      named_type<T, tTag>::template is_equivalent<T2>;
  static constexpr bool optional = true;
};
template <template_string tTag> struct optional_auto_arg {
  template <typename T2>
  static constexpr bool is_equivalent =
      named_auto<tTag>::template is_equivalent<T2>;
  static constexpr bool optional = true;
};

namespace details {
template <typename TReq, typename... TArgs>
  requires(requires(TReq) {
    {
      TReq::template is_equivalent<named_arg_t<"na", int>>
      } -> std::convertible_to<bool>;
    { TReq::optional } -> std::convertible_to<bool>;
  })
constexpr bool is_equivalent_with_any =
    TReq::optional || (TReq::template is_equivalent<TArgs> || ...);

template <typename TArg, typename... TReqs>
  requires(requires(TReqs) {
    {
      TReqs::template is_equivalent<named_arg_t<"na", int>>
      } -> std::convertible_to<bool>;
  } && ...)
constexpr bool is_any_equivalent_with =
    (TReqs::template is_equivalent<TArg> || ...);

} // namespace details

template <typename... Ts>
  requires(requires(Ts) {
    {
      Ts::template is_equivalent<named_arg_t<"na", int>>
      } -> std::convertible_to<bool>;
  } && ...)
struct arg_list {
private:
  template <typename... Ts2> static constexpr bool fullfilled_by_impl() {
    constexpr auto is_fullfilled_f = []<typename T>(T const &) {
      if constexpr (details::is_equivalent_with_any<T, Ts2...>)
        return std::true_type();
      else
        return std::false_type();
    };
    constexpr auto fullfills_any_f = []<typename T2>(T2 const &) {
      if constexpr (details::is_any_equivalent_with<T2, Ts...>)
        return std::true_type{};
      else
        return std::false_type{};
    };
    if constexpr ((decltype(is_fullfilled_f(
                      std::declval<Ts>()))::value &&...) &&
                  (decltype(fullfills_any_f(std::declval<Ts2>()))::value &&...))
      return true;
    else
      return false;
  }

public:
  template <typename... Ts2>
  static constexpr bool fullfilled_by = fullfilled_by_impl<Ts2...>();
  // std::remove_cvref_t<decltype(fullfilled_by_impl<Ts2...>)>::value;
};

template <template_string tTag, arg_with_any_name T, arg_with_any_name T2,
          arg_with_any_name... Ts>
  requires(is_tagged_with<T, tTag> ||
           is_tagged_with<std::remove_cvref_t<T2>, tTag> ||
           (is_tagged_with<Ts, tTag> || ...))
constexpr decltype(auto) get(T &&t, T2 &&t2, Ts &&...ts) {
  if constexpr (is_tagged_with<T, tTag>) {
    unused(t2);
    return get<tTag>(std::forward<T>(t));
  } else {
    unused(t);
    return get<tTag>(std::forward<T2>(t2), std::forward<Ts>(ts)...);
  }
}

template <template_string tTag, arg_with_any_name... Ts>
constexpr bool arg_provided = ((is_tagged_with<Ts, tTag> || ...));

template <template_string tTag, typename T, arg_with_any_name... Ts>
constexpr decltype(auto) get_or(T &&default_return, Ts &&...args) {
  if constexpr ((is_tagged_with<Ts, tTag> || ...)) {
    unused(default_return);
    return get<tTag>(std::forward<Ts>(args)...);
  } else {
    unused(args...);
    return std::forward<T>(default_return);
  }
}

template <template_string... tTags, typename... Ts>
struct named_tuple<named_arg_t<tTags, Ts>...> {
private:
  using this_type = named_tuple<named_arg_t<tTags, Ts>...>;
  using data_t = std::tuple<named_arg_t<tTags, Ts>...>;
  data_t data_;

public:
  static constexpr template_string_list_t<tTags...> arg_list_{};
  constexpr named_tuple() noexcept(
      std::is_nothrow_default_constructible_v<std::tuple<
          Ts...>>) requires(std::is_default_constructible_v<data_t>) = default;

  template <typename... Us>
    requires(sizeof...(Us) == sizeof...(Ts) &&
             (std::is_constructible_v<Us, Ts> && ...))
  constexpr named_tuple(Us &&...args) : data_(std::forward<Us>(args)...) {}

  constexpr named_tuple(named_arg_t<tTags, Ts> const &...ts) noexcept(
      (std::is_nothrow_copy_constructible_v<Ts> && ...))
      : data_(ts.value()...) {}
  constexpr named_tuple(named_arg_t<tTags, Ts> &&...ts) noexcept(
      (std::is_nothrow_copy_constructible_v<Ts> && ...))
      : data_(std::move(ts).value()...) {}

  template <named_tuple_like TTuple>
    requires(tuple_size_v<TTuple> == tuple_size_v<named_tuple> &&
             !std::is_same_v<TTuple, named_tuple> &&
             ((std::is_convertible_v<
                 named_tuple_element_t<tTags, TTuple>,
                 named_tuple_element_t<tTags, named_tuple>>)&&...))
  constexpr named_tuple(TTuple const &t) : data_(get<tTags>(t)...) {}

  template <named_tuple_like TTuple>
    requires(tuple_size_v<TTuple> == tuple_size_v<named_tuple> &&
             !std::is_same_v<TTuple, named_tuple> &&
             (std::is_convertible_v<named_tuple_element_t<tTags, TTuple>,
                                    named_tuple_element_t<tTags, this_type>> &&
              ...) &&
             details::is_tuple_convertible_v<std::remove_cvref_t<TTuple>,
                                             named_tuple>)
  named_tuple &operator=(TTuple const &t) {
    unused(((get<tTags>(*this) = get<tTags>(t), 0) + ...));
    return *this;
  }

  template <template_string tToGet>
  constexpr named_tuple_element_t<tToGet, this_type> &_get_impl() {
    constexpr auto index =
        details::index_of_template_string<tToGet, tTags...>();
    static_assert(index < sizeof...(tTags),
                  "'get<tToGet>: string 'tToGet' not found!");
    return get<index>(data_);
  }
};
template <template_string... TTag, typename... Ts>
named_tuple(named_arg_t<TTag, Ts> const &...)
    -> named_tuple<named_arg_t<TTag, Ts>...>;

template <template_string tName, typename T>
constexpr named_arg_t<tName, std::unwrap_ref_decay_t<T>> named_arg(T &&t) {
  return named_arg_t<tName, std::unwrap_ref_decay_t<T>>{std::forward<T>(t)};
}

template <typename T> constexpr bool is_named_arg = false;

template <template_string tName, typename T>
constexpr bool is_named_arg<named_arg_t<tName, T>> = true;

template <template_string tTag, template_string... tTags, typename... Ts>
struct named_tuple_element<tTag, named_tuple<named_arg_t<tTags, Ts>...>> {
private:
  static constexpr auto index_ =
      details::index_of_template_string<tTag, tTags...>();
  static_assert(index_ < sizeof...(Ts), "Tag not in tuple");

public:
  using type =
      std::tuple_element_t<details::index_of_template_string<tTag, tTags...>(),
                           std::tuple<Ts...>>;
};

template <template_string tTag, typename T>
using named_tuple_element_t = typename named_tuple_element<tTag, T>::type;

template <typename... Ts>
  requires(is_named_arg<std::remove_cvref_t<Ts>> &&...)
constexpr named_tuple<std::remove_cvref_t<Ts>...>
make_named_args(Ts &&...args) {
  return named_tuple<std::remove_reference_t<Ts>...>{std::forward<Ts>(args)...};
}

template <template_string tTag, template_string... tTags, typename... Ts>
constexpr named_tuple_element_t<tTag, named_tuple<named_arg_t<tTags, Ts>...>> &
get(named_tuple<named_arg_t<tTags, Ts>...> &t) {
  return static_cast<
      named_tuple_element_t<tTag, named_tuple<named_arg_t<tTags, Ts>...>> &>(
      t.template _get_impl<tTag>());
}

template <template_string tTag, template_string... tTags, typename... Ts>
constexpr named_tuple_element_t<tTag,
                                named_tuple<named_arg_t<tTags, Ts>...>> const &
get(named_tuple<named_arg_t<tTags, Ts>...> const &t) {
  return static_cast<named_tuple_element_t<
      tTag, named_tuple<named_arg_t<tTags, Ts>...>> const &>(
      get<tTag>(const_cast<std::remove_cvref_t<decltype(t)> &>(t)));
}

template <template_string tTag, template_string... tTags, typename... Ts>
constexpr named_tuple_element_t<tTag, named_tuple<named_arg_t<tTags, Ts>...>> &&
get(named_tuple<named_arg_t<tTags, Ts>...> &&t) {
  return static_cast<
      named_tuple_element_t<tTag, named_tuple<named_arg_t<tTags, Ts>...>> &&>(
      get<tTag>(t));
}

template <template_string tTag, template_string... tTags, typename... Ts>
constexpr named_tuple_element_t<tTag,
                                named_tuple<named_arg_t<tTags, Ts>...>> const &&
get(named_tuple<named_arg_t<tTags, Ts>...> const &&t) {
  return static_cast<named_tuple_element_t<
      tTag, named_tuple<named_arg_t<tTags, Ts>...>> const &&>(get<tTag>(t));
}

template <template_string tTag, typename TTuple>
constexpr bool contains_arg_v = false;

template <template_string tTag, typename TTuple>
  requires(!std::is_same_v<TTuple, std::remove_cvref_t<TTuple>>)
constexpr bool contains_arg_v<tTag, TTuple> =
    contains_arg_v<tTag, std::remove_cvref_t<TTuple>>;

template <template_string tTag, typename TTuple>
  requires requires() { {TTuple::arg_list_}; }
constexpr bool contains_arg_v<tTag, TTuple> =
    details::index_of_template_string_list<tTag>(TTuple::arg_list_) !=
    TTuple::arg_list_.size();

template <typename TTuple, template_string... tTags>
class named_tuple_slice_view {
  TTuple tuple_;

public:
  static constexpr template_string_list_t<tTags...> arg_list_{};
  explicit constexpr named_tuple_slice_view(TTuple &&t)
      : tuple_(std::forward<TTuple>(t)) {}

  template <template_string tTag>
  friend constexpr bool contains_arg(named_tuple_slice_view const &) noexcept {
    return details::index_of_template_string<tTag, tTags...>() !=
           sizeof...(tTags);
  }

  template <template_string tTag>
  friend constexpr decltype(auto) get(named_tuple_slice_view &tuple) {
    static_assert(contains_arg_v<tTag, named_tuple_slice_view>,
                  "Tag not in slice");
    return get<tTag>(tuple.tuple_);
  }
  template <template_string tTag>
  friend constexpr decltype(auto) get(named_tuple_slice_view const &tuple) {
    static_assert(contains_arg_v<tTag, named_tuple_slice_view>,
                  "Tag not in slice");
    return get<tTag>(tuple.tuple_);
  }
  template <template_string tTag>
  friend constexpr decltype(auto) get(named_tuple_slice_view &&tuple) {
    static_assert(contains_arg_v<tTag, named_tuple_slice_view>,
                  "Tag not in slice");
    return get<tTag>(std::move(tuple.tuple_));
  }
};
template <template_string tTag, typename TTuple, template_string... tTags>
struct named_tuple_element<tTag, named_tuple_slice_view<TTuple, tTags...>>
    : named_tuple_element<tTag, std::remove_reference_t<TTuple>> {};

template <template_string... tTags, typename TTuple>
constexpr named_tuple_slice_view<TTuple, tTags...> get_slice_view(TTuple &&t) {
  return named_tuple_slice_view<TTuple, tTags...>{std::forward<TTuple>(t)};
}

template <template_string... tTags, typename TTuple>
constexpr named_tuple_slice_view<TTuple, tTags...>
get_slice_view(TTuple &&t, template_string_list_t<tTags...>) {
  return get_slice_view<tTags...>(std::forward<TTuple>(t));
}

namespace details {
template <template_string... tTags, typename... Ts, typename TTuple>
  requires(sizeof...(tTags) == tuple_size_v<TTuple> &&
           ((std::is_convertible_v<named_arg_t<tTags, Ts>,
                                   named_tuple_element_t<tTags, TTuple>>)&&...))
struct is_tuple_convertible<named_tuple<named_arg_t<tTags, Ts>...>, TTuple>
    : std::true_type {};
template <typename T1, typename T2>
struct is_tuple_convertible<const T1, T2> : is_tuple_convertible<T1, T2> {};
template <typename T1, typename T2>
struct is_tuple_convertible<volatile T1, T2> : is_tuple_convertible<T1, T2> {};
template <typename T1, typename T2>
struct is_tuple_convertible<const volatile T1, T2>
    : is_tuple_convertible<T1, T2> {};

template <template_string_list_c> struct get_slice_help;

template <template_string... tTags>
struct get_slice_help<template_string_list_t<tTags...>> {
  static constexpr decltype(auto) call(auto const &t) {
    return get_slice_view<tTags...>(t);
  }
};

template <typename> struct tuple_transform_constexpr_members {};

template <typename TTuple>
  requires requires(TTuple) { TTuple::arg_list_; }
struct tuple_transform_constexpr_members<TTuple> {
  static constexpr template_string_list_t arg_list_ = TTuple::arg_list_;
};

template <typename T>
concept non_void = !std::is_same_v<T, void>;

template <typename TFunc, typename TTuple, template_string... tNames>
  requires(
      (requires(
          TFunc f,
          named_tuple_element_t<tNames, std::remove_reference_t<TTuple>> e) {
        {
          f(std::forward<
                named_tuple_element_t<tNames, std::remove_reference_t<TTuple>>>(
                e),
            tNames)
          } -> non_void;
      }) &&
      ...)
/*requires requires(TFunc f, TTuple &&t) {
  std::tuple(f(get<tNames>(std::forward<TTuple>(t)), tNames)...);
}*/
constexpr void call_for_each(TFunc &&, TTuple &&,
                             template_string_list_t<tNames...>) {}

template <typename TFunc, typename TTuple>
concept func_works_with_tuple_c = requires(TFunc f, TTuple t) {
  { contained_tags<std::remove_cvref_t<TTuple>> } -> template_string_list_c;
  call_for_each(f, t, contained_tags<std::remove_cvref_t<TTuple>>);
};

template <typename TTuple, func_works_with_tuple_c<TTuple> TFunc>
class tuple_transform_t : public tuple_transform_constexpr_members<TTuple> {
  TTuple tuple_;
  TFunc f_;

public:
  constexpr tuple_transform_t(TFunc f, TTuple &&t) noexcept(
      std::is_nothrow_move_constructible_v<TFunc>)
      : f_(std::move(f)), tuple_(std::forward<TTuple>(t)) {}
  template <template_string tTag, typename TFunc2, typename TTuple2>
  friend constexpr decltype(auto) get(tuple_transform_t<TTuple2, TFunc2> &t);
  template <template_string tTag, typename TFunc2, typename TTuple2>
  friend constexpr decltype(auto)
  get(tuple_transform_t<TTuple2, TFunc2> const &t);
  template <template_string tTag, typename TFunc2, typename TTuple2>
  friend constexpr decltype(auto) get(tuple_transform_t<TTuple2, TFunc2> &&t);
  template <std::size_t tIndex, typename TFunc2, typename TTuple2>
  friend constexpr decltype(auto) get(tuple_transform_t<TTuple2, TFunc2> &t);
};

template <template_string tTag, typename TFunc2, typename TTuple2>
constexpr decltype(auto) get(tuple_transform_t<TTuple2, TFunc2> &t) {
  using type = decltype(get<tTag>(t.tuple_));
  return t.f_(get<tTag>(t.tuple_), tTag);
}
template <template_string tTag, typename TFunc2, typename TTuple2>
constexpr decltype(auto) get(tuple_transform_t<TTuple2, TFunc2> const &t) {
  using type = decltype(get<tTag>(t.tuple_));
  return t.f_(get<tTag>(t.tuple_), tTag);
}
template <template_string tTag, typename TFunc2, typename TTuple2>
constexpr decltype(auto) get(tuple_transform_t<TTuple2, TFunc2> &&t) {
  using type = decltype(get<tTag>(std::move(t.tuple_)));
  return t.f_(get<tTag>(std::move(t.tuple_)), tTag);
}
template <std::size_t tIndex, typename TFunc2, typename TTuple2>
constexpr decltype(auto) get(tuple_transform_t<TTuple2, TFunc2> &t) {
  return t.f_(get<tIndex>(t.tuple_));
}
} // namespace details

template <template_string_list_c tTags, typename TTuple>
constexpr auto get_slice_view(TTuple const &t) {
  return details::get_slice_help<tTags>::call(t);
}

namespace details {
template <typename T> class named_initializer_list_t {
  using span_t = std::span<T const>;
  span_t data_;

public:
  using iterator = T const *;
  using const_iterator = T const *;
  using size_type = typename span_t::size_type;
  using value_type = T;
  using reference = T const &;
  using const_reference = T const &;
  constexpr named_initializer_list_t() = default;
  constexpr explicit(false)
      named_initializer_list_t(std::initializer_list<T> list)
      : data_(list.begin(), list.end()) {}

  constexpr iterator begin() const noexcept { return data_.data(); }
  constexpr iterator end() const noexcept {
    return data_.data() + ssize(data_);
  }
  constexpr size_type size() const noexcept { return data_.size(); }

  template <std::constructible_from<iterator, iterator> T2>
  constexpr operator T2() const {
    return T2(begin(), end());
  }
};

template <template_string tName> struct to_named_arg_t {
  template <typename T> constexpr auto operator()(T &&v) const noexcept {
    return named_arg<tName>(std::forward<T>(v));
  }
  template <typename T> constexpr auto operator=(T &&v) const noexcept {
    return named_arg<tName>(std::forward<T>(v));
  }
  template <typename T>
  constexpr named_arg_t<tName, named_initializer_list_t<T>>
  operator()(std::initializer_list<T> v) const noexcept {
    return named_arg<tName>(named_initializer_list_t<T>(v));
  }
  template <typename T>
  constexpr named_arg_t<tName, named_initializer_list_t<T>>
  operator=(std::initializer_list<T> v) const noexcept {
    return named_arg<tName>(named_initializer_list_t<T>(v));
  }
};
template <template_string tName> struct get_named_arg_t {
  template <typename... Ts>
  constexpr decltype(auto) operator()(Ts &&...t) const noexcept {
    return get<tName>(std::forward<Ts>(t)...);
  }
};

template <typename, typename> struct named_tuple_cat_helper;
template <template_string... tTags1, typename... Ts1, template_string... tTags2,
          typename... Ts2>
struct named_tuple_cat_helper<named_tuple<named_arg_t<tTags1, Ts1>...>,
                              named_tuple<named_arg_t<tTags2, Ts2>...>> {
  using type =
      named_tuple<named_arg_t<tTags1, Ts1>..., named_arg_t<tTags2, Ts2>...>;

  template <typename T1, typename T2>
  static constexpr type _cat(T1 &&t1, T2 &&t2) {
    return named_tuple{
        named_arg_t<tTags1, Ts1>(get<tTags1>(std::forward<T1>(t1)))...,
        named_arg_t<tTags2, Ts2>(get<tTags2>(std::forward<T2>(t2)))...};
  }
};

template <typename TTuple, template_string... tTags>
constexpr decltype(auto) apply_impl_(auto &&callable, TTuple &&t,
                                     template_string_list_t<tTags...> const &) {
  callable(get<tTags>(std::forward<TTuple>(t))...);
}
template <typename T>
constexpr std::tuple<T &&> construct_extract(T &&v) noexcept {
  return std::forward_as_tuple(std::forward<T>(v));
}
template <typename T>
constexpr std::tuple<T const *, T const *>
construct_extract(named_initializer_list_t<T> v) noexcept {
  return {v.begin(), v.end()};
}
} // namespace details

template <typename... Ts> class construct {
  using tuple_t = decltype(std::tuple_cat(
      details::construct_extract(std::declval<Ts &&>())...));
  tuple_t data_;

public:
  explicit constexpr construct(Ts &&...args)
      : data_(std::tuple_cat(
            details::construct_extract(std::forward<Ts>(args))...)) {}

  template <typename T> explicit constexpr operator T() const {
    return std::apply(
        []<typename... Ts2>(Ts2 && ...args) {
          return T(std::forward<Ts2>(args)...);
        },
        data_);
  }
};

template <typename T1, typename T2>
constexpr
    typename details::named_tuple_cat_helper<std::remove_cvref_t<T1>,
                                             std::remove_cvref_t<T2>>::type
    named_tuple_cat(T1 &&t1, T2 &&t2) {
  return details::named_tuple_cat_helper<
      std::remove_cvref_t<T1>,
      std::remove_cvref_t<T2>>::_cat(std::forward<T1>(t1),
                                     std::forward<T2>(t2));
}

template <typename T> constexpr std::remove_cvref_t<T> named_tuple_cat(T &&t) {
  return std::forward<T>(t);
}

template <typename T1, typename T2, typename... Ts>
constexpr auto named_tuple_cat(T1 &&t1, T2 &&t2, Ts &&...ts) {
  return named_tuple_cat(
      named_tuple_cat(std::forward<T1>(t1), std::forward<T2>(t2)),
      std::forward<Ts>(ts)...);
}

template <template_string tTag, typename TTuple, typename TFunc>
struct named_tuple_element<tTag, details::tuple_transform_t<TTuple, TFunc>> {
  using type = decltype(std::declval<TFunc>()(
      std::declval<
          named_tuple_element_t<tTag, std::remove_reference_t<TTuple>>>()));
};
template <template_string tTag, typename TTuple, typename TFunc>
struct named_tuple_element<tTag,
                           details::tuple_transform_t<TTuple, TFunc> const>
    : named_tuple_element<tTag, details::tuple_transform_t<TTuple, TFunc>> {};
template <template_string tTag, typename TTuple, typename TFunc>
struct named_tuple_element<tTag,
                           details::tuple_transform_t<TTuple, TFunc> volatile>
    : named_tuple_element<tTag, details::tuple_transform_t<TTuple, TFunc>> {};
template <template_string tTag, typename TTuple, typename TFunc>
struct named_tuple_element<
    tTag, details::tuple_transform_t<TTuple, TFunc> const volatile>
    : named_tuple_element<tTag, details::tuple_transform_t<TTuple, TFunc>> {};

template <template_string tTag, template_string... tTags, typename... Ts>
constexpr bool
contains_arg(named_tuple<named_arg_t<tTags, Ts>...> const &) noexcept {
  return details::index_of_template_string<tTag, tTags...>() !=
         sizeof...(tTags);
}

template <template_string tTag, typename TTuple, template_string... tTags>
constexpr bool
contains_arg(named_tuple_slice_view<TTuple, tTags...> const &) noexcept {
  return details::index_of_template_string<tTag, tTags...>() !=
         sizeof...(tTags);
}

template <template_string tTag, typename TFunc, typename TTuple>
constexpr bool
contains_arg(details::tuple_transform_t<TTuple, TFunc> const &) noexcept {
  return contains_arg_v<tTag, TTuple>;
}

template <typename TNamedTuple>
  requires requires() {
    { std::remove_cvref_t<TNamedTuple>::arg_list_ } -> template_string_list_c;
  }
constexpr decltype(auto) apply(auto &&callable, TNamedTuple &&t) noexcept {
  return details::apply_impl_(callable, std::forward<TNamedTuple>(t),
                              t.arg_list_);
}

template <typename TNamedTuple, template_string... tArgOrder>
  requires(contains_arg_v<tArgOrder, std::remove_cvref_t<TNamedTuple>> &&...)
constexpr decltype(auto)
apply(auto &&callable, TNamedTuple &&t,
      template_string_list_t<tArgOrder...> arg_list) noexcept {
  return details::apply_impl_(callable, std::forward<TNamedTuple>(t), arg_list);
}

template < // named_tuple_like TTuple,
    typename TTuple,
    // details::func_works_with_tuple_c<TTuple> TFunc
    typename TFunc>
constexpr details::tuple_transform_t<TTuple, TFunc> transform(TFunc f,
                                                              TTuple &&t) {
  return {std::move(f), std::forward<TTuple>(t)};
}

namespace tuple_literals {
template <template_string tTag>
constexpr details::to_named_arg_t<tTag> operator"" _na() noexcept {
  return {};
}
template <template_string tTag>
constexpr details::get_named_arg_t<tTag> operator"" _from() noexcept {
  return {};
}
} // namespace tuple_literals

} // namespace dooc

namespace std {
template <dooc::template_string... tTags, typename... Ts>
struct tuple_size<dooc::named_tuple<dooc::named_arg_t<tTags, Ts>...>>
    : integral_constant<std::size_t, sizeof...(tTags)> {};

template <typename TTuple, dooc::template_string... tTags>
struct tuple_size<dooc::named_tuple_slice_view<TTuple, tTags...>>
    : integral_constant<std::size_t, sizeof...(tTags)> {};

template <typename TFunc, typename TTuple>
struct tuple_size<dooc::details::tuple_transform_t<TTuple, TFunc>>
    : tuple_size<TTuple> {};
} // namespace std

#endif
