#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#
        #define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
        
        #define SAFE_EXIT(message) do                     {                      perror(message);   exit(1);           } while (0)
        
        #define SAFE_ACCESS_ARRAY(arr, index)                              ({                                                             typeof(arr[0]) _result;                                    if ((index) < 0 || (size_t)(index) >= ARRAY_SIZE(arr))     {                                                          fprintf(stderr, "Error: Array index out of bounds\n"); SAFE_EXIT("Exiting due to array index out of bounds"); }                                                          _result = (arr)[index];                                    _result;                                                   })
        
        #define SAFE_SET_ARRAY(arr, index, value)                          do                                                             {                                                              if ((index) < 0 || (size_t)(index) >= ARRAY_SIZE(arr))     {                                                          fprintf(stderr, "Error: Array index out of bounds\n"); SAFE_EXIT("Exiting due to array index out of bounds"); }                                                          (arr)[index] = value;                                      } while (0)
typedef enum Color {
Color_Red,
Color_Green,
Color_Blue,
} Color;
typedef enum TrafficLight {
TrafficLight_Red,
TrafficLight_Yellow,
TrafficLight_Green,
} TrafficLight;
void print_color( Color c){
switch (c) {
     case Color_Red:
{
printf("%s\n", "Red");};
break;
     case Color_Green:
{
printf("%s\n", "Green");};
break;
     case Color_Blue:
{
printf("%s\n", "Blue");};
break;
}
}
int main() {Color c = Color_Red;
print_color(c);
print_color(Color_Blue);
c = Color_Green;
print_color(c);
return 0;
}
