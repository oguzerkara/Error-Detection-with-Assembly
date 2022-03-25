Parity Bits is a simple error detection method. A check bit is added to end of the data. For the check bit decision, total number of 'one's in the data is counted.

In odd parity bit case, when the total number of 1s is odd a 0 is added, and for an even number a 1 is added to end.

The even parity bit is vice versa. When the total number of 1s are odd a 1 is added, for even a 0 added to end.

In this work there are totally 4 modes of operation, and modes can be used by 2 switches.  Use of parity bits are separated with 2 different modes (node 0 and 1). Input for the parity bits is  from first switch set. There is also another switch set for a second number. In mode 2 the larger one of these inputs is demonstrated on LCD screen. In the case of equality, there become a 'EQ' indication on the screen. For the last mode (mode 3), the difference of the 2 inputs are shown on LCD with both its hexadecimal and binary values. The code is allowing to do the subtraction of smaller number from larger one.

Mode: 0 --> odd parity bit appended
Mode: 1 --> even parity bit appended
Mode: 2 --> Larger of the both input data is demonstrated on LCD screen with its hexadecimal response.
Mode: 3 --> subtraction of smaller from larger in positive logic sense. 
