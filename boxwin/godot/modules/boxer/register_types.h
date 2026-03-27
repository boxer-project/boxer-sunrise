#ifndef GDBOXER_REGISTER_TYPES_H
#define GDBOXER_REGISTER_TYPES_H

#ifdef BOXER_GDEXTENSION
#include <godot_cpp/core/class_db.hpp>
using namespace godot;
#else
#include "modules/register_module_types.h"
#include "core/object/class_db.h"
#endif


void initialize_boxer_module(ModuleInitializationLevel p_level);
void uninitialize_boxer_module(ModuleInitializationLevel p_level);

#endif // GDBOXER_REGISTER_TYPES_H
