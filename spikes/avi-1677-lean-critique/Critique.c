// Lean compiler output
// Module: Critique
// Imports: public import Init
#include <lean/lean.h>
#if defined(__clang__)
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wunused-label"
#elif defined(__GNUC__) && !defined(__CLANG__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#ifdef __cplusplus
extern "C" {
#endif
uint8_t lean_uint8_dec_lt(uint8_t, uint8_t);
LEAN_EXPORT uint64_t l_confBonus(uint8_t);
LEAN_EXPORT lean_object* l_confBonus___boxed(lean_object*);
uint8_t lean_uint8_dec_eq(uint8_t, uint8_t);
LEAN_EXPORT uint64_t l_crossFiletypeBonus(uint8_t);
LEAN_EXPORT lean_object* l_crossFiletypeBonus___boxed(lean_object*);
LEAN_EXPORT uint64_t l_crossCommunityBonus(uint8_t);
LEAN_EXPORT lean_object* l_crossCommunityBonus___boxed(lean_object*);
uint8_t lean_uint64_dec_eq(uint64_t, uint64_t);
LEAN_EXPORT uint64_t l_semBonus(uint8_t, uint64_t);
LEAN_EXPORT lean_object* l_semBonus___boxed(lean_object*, lean_object*);
uint64_t lean_uint64_add(uint64_t, uint64_t);
LEAN_EXPORT uint64_t graphos_surprise_score(uint8_t, uint8_t, uint8_t, uint8_t);
LEAN_EXPORT lean_object* l_surpriseScore___boxed(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT uint32_t graphos_lean_critique_abi_version;
LEAN_EXPORT uint64_t l_confBonus(uint8_t x_1) {
_start:
{
uint8_t x_2; uint8_t x_3; 
x_2 = 3;
x_3 = lean_uint8_dec_lt(x_1, x_2);
if (x_3 == 0)
{
uint8_t x_4; uint8_t x_5; 
x_4 = 8;
x_5 = lean_uint8_dec_lt(x_1, x_4);
if (x_5 == 0)
{
uint64_t x_6; 
x_6 = 1;
return x_6;
}
else
{
uint64_t x_7; 
x_7 = 2;
return x_7;
}
}
else
{
uint64_t x_8; 
x_8 = 3;
return x_8;
}
}
}
LEAN_EXPORT lean_object* l_confBonus___boxed(lean_object* x_1) {
_start:
{
uint8_t x_2; uint64_t x_3; lean_object* x_4; 
x_2 = lean_unbox(x_1);
x_3 = l_confBonus(x_2);
x_4 = lean_box_uint64(x_3);
return x_4;
}
}
LEAN_EXPORT uint64_t l_crossFiletypeBonus(uint8_t x_1) {
_start:
{
uint8_t x_2; uint8_t x_3; 
x_2 = 1;
x_3 = lean_uint8_dec_eq(x_1, x_2);
if (x_3 == 0)
{
uint64_t x_4; 
x_4 = 0;
return x_4;
}
else
{
uint64_t x_5; 
x_5 = 2;
return x_5;
}
}
}
LEAN_EXPORT lean_object* l_crossFiletypeBonus___boxed(lean_object* x_1) {
_start:
{
uint8_t x_2; uint64_t x_3; lean_object* x_4; 
x_2 = lean_unbox(x_1);
x_3 = l_crossFiletypeBonus(x_2);
x_4 = lean_box_uint64(x_3);
return x_4;
}
}
LEAN_EXPORT uint64_t l_crossCommunityBonus(uint8_t x_1) {
_start:
{
uint8_t x_2; uint8_t x_3; 
x_2 = 1;
x_3 = lean_uint8_dec_eq(x_1, x_2);
if (x_3 == 0)
{
uint64_t x_4; 
x_4 = 0;
return x_4;
}
else
{
uint64_t x_5; 
x_5 = 1;
return x_5;
}
}
}
LEAN_EXPORT lean_object* l_crossCommunityBonus___boxed(lean_object* x_1) {
_start:
{
uint8_t x_2; uint64_t x_3; lean_object* x_4; 
x_2 = lean_unbox(x_1);
x_3 = l_crossCommunityBonus(x_2);
x_4 = lean_box_uint64(x_3);
return x_4;
}
}
LEAN_EXPORT uint64_t l_semBonus(uint8_t x_1, uint64_t x_2) {
_start:
{
uint8_t x_3; uint8_t x_4; 
x_3 = 1;
x_4 = lean_uint8_dec_eq(x_1, x_3);
if (x_4 == 0)
{
uint64_t x_5; 
x_5 = 0;
return x_5;
}
else
{
uint64_t x_6; uint8_t x_7; 
x_6 = 3;
x_7 = lean_uint64_dec_eq(x_2, x_6);
if (x_7 == 0)
{
uint64_t x_8; uint8_t x_9; 
x_8 = 2;
x_9 = lean_uint64_dec_eq(x_2, x_8);
if (x_9 == 0)
{
return x_8;
}
else
{
return x_6;
}
}
else
{
uint64_t x_10; 
x_10 = 4;
return x_10;
}
}
}
}
LEAN_EXPORT lean_object* l_semBonus___boxed(lean_object* x_1, lean_object* x_2) {
_start:
{
uint8_t x_3; uint64_t x_4; uint64_t x_5; lean_object* x_6; 
x_3 = lean_unbox(x_1);
x_4 = lean_unbox_uint64(x_2);
lean_dec_ref(x_2);
x_5 = l_semBonus(x_3, x_4);
x_6 = lean_box_uint64(x_5);
return x_6;
}
}
LEAN_EXPORT uint64_t graphos_surprise_score(uint8_t x_1, uint8_t x_2, uint8_t x_3, uint8_t x_4) {
_start:
{
uint64_t x_5; uint64_t x_6; uint64_t x_7; uint64_t x_8; uint64_t x_9; uint64_t x_10; uint64_t x_11; 
x_5 = l_confBonus(x_1);
x_6 = l_crossFiletypeBonus(x_2);
x_7 = lean_uint64_add(x_5, x_6);
x_8 = l_crossCommunityBonus(x_3);
x_9 = lean_uint64_add(x_7, x_8);
x_10 = l_semBonus(x_4, x_5);
x_11 = lean_uint64_add(x_9, x_10);
return x_11;
}
}
LEAN_EXPORT lean_object* l_surpriseScore___boxed(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4) {
_start:
{
uint8_t x_5; uint8_t x_6; uint8_t x_7; uint8_t x_8; uint64_t x_9; lean_object* x_10; 
x_5 = lean_unbox(x_1);
x_6 = lean_unbox(x_2);
x_7 = lean_unbox(x_3);
x_8 = lean_unbox(x_4);
x_9 = graphos_surprise_score(x_5, x_6, x_7, x_8);
x_10 = lean_box_uint64(x_9);
return x_10;
}
}
static uint32_t _init_graphos_lean_critique_abi_version(void) {
_start:
{
uint32_t x_1; 
x_1 = 0;
return x_1;
}
}
lean_object* initialize_Init(uint8_t builtin);
static bool _G_initialized = false;
LEAN_EXPORT lean_object* initialize_Critique(uint8_t builtin) {
lean_object * res;
if (_G_initialized) return lean_io_result_mk_ok(lean_box(0));
_G_initialized = true;
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
graphos_lean_critique_abi_version = _init_graphos_lean_critique_abi_version();
return lean_io_result_mk_ok(lean_box(0));
}
#ifdef __cplusplus
}
#endif
