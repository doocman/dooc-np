//
// Created by doocm on 2022-07-16.
//

#ifndef DOOC_NP_DOOC_CONCEPTS_HPP
#define DOOC_NP_DOOC_CONCEPTS_HPP

#include <type_traits>

namespace dooc {

template<typename T>
concept qualified_type = std::is_const_v<T> || std::is_volatile_v<T>;
template<typename T>
concept reference_type = std::is_reference_v<T>;
template<typename T>
concept ref_or_qualified_type = qualified_type<T> || reference_type<T>;
template<typename T>
concept pure_type = !ref_or_qualified_type<T>;

}

#endif
