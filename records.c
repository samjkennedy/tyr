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
typedef struct Point
{
        unsigned long x;
        unsigned long y;
} Point;
typedef struct Line
{
        Point a;
        Point b;
} Line;
Point make_point(unsigned long x, unsigned long y)
{
        return (Point){
            x, y};
}
int main()
{
        Point p1 = (Point){
            4, 5};
        Point p2 = make_point(2, 6);
        Line l = (Line){
            p1, p2};
        printf("%lu\n", l.a.x + l.b.x);
        printf("%lu\n", l.a.y + l.b.y);
        return 0;
}
