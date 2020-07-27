
#define must_use __attribute__ ((warn_unused_result))

#define bytes_per_value 8 //TODO, compute

#define die { printf("die: %s:%d (%s)\n", __FILE__, __LINE__, __FUNCTION__); exit(1); }

typedef void* value;
typedef int bool_t;

typedef void* (*func_p)(void);

typedef func_p (*func_p1)(void);

#define False 0
#define True 1

extern value lits[];
extern func_p1 native[];

extern func_p must_use bytecode(char*);

// specific code sequences for the nfib example

extern func_p U5(void);
extern func_p V(void);
extern func_p W(void);
extern func_p X(void);
extern func_p Y(void);
extern func_p Z(void);
