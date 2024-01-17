#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

#define SAFE_EXIT(message)       \
        do                       \
        {                        \
                perror(message); \
                exit(1);         \
        } while (0)

#define SAFE_ACCESS_ARRAY(arr, index) ({                                                             typeof(arr[0]) _result;                                    if ((index) < 0 || (size_t)(index) >= ARRAY_SIZE(arr))     {                                                          fprintf(stderr, "Error: Array index out of bounds\n"); SAFE_EXIT("Exiting due to array index out of bounds"); }                                                          _result = (arr)[index];                                    _result; })

#define SAFE_SET_ARRAY(arr, index, value)                                      \
        do                                                                     \
        {                                                                      \
                if ((index) < 0 || (size_t)(index) >= ARRAY_SIZE(arr))         \
                {                                                              \
                        fprintf(stderr, "Error: Array index out of bounds\n"); \
                        SAFE_EXIT("Exiting due to array index out of bounds"); \
                }                                                              \
                (arr)[index] = value;                                          \
        } while (0)
typedef struct Vec
{
        float x;
        float y;
} Vec;
float sqr_len(Vec v)
{
        return v.x * v.x + v.y * v.y;
}
Vec scale(Vec v, float factor)
{
        return (Vec){
            v.x * factor, v.y * factor};
}
int main()
{
        Vec v = (Vec){
            2, 3};
        printf("%.2f\n", sqr_len(scale(v, 2)));
        return 0;
}
