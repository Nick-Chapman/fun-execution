
#define bytes_per_value 8 //TODO, compute

typedef void* value;
typedef int bool_t;

#define False 0
#define True 1

extern const bool_t config_fvs_on_stack;

extern value lits[];
extern char* prog[];
