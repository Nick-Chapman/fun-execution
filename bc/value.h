
#define bytes_per_value 8 //TODO, compute

typedef void* value;
typedef int bool_t;

typedef void* (*func_p)(void);

#define False 0
#define True 1

extern const bool_t config_fvs_on_stack;

extern value lits[];
extern char* prog[];

extern func_p native[];


// specific code sequences for the nfib example

extern func_p V(void);
extern func_p W(void);
extern func_p X(void);
extern func_p Y(void);
extern func_p Z(void);

extern func_p X_fos(void);
extern func_p Y_fos(void);
