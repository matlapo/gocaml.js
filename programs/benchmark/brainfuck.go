package main

// Brainfuck interepreter with a brainfuck program to print the squares from 1 to 1000
// However, golitec does not support printing runes as characters to it prints the int value of the chars.
// Does it a 100 times because the codegen is too fast

func execute(program [1000]rune) string {
  var mem [1000]int
  var ptr = 0
  var pc = 0
  var output = ""
  stop := false
  for !stop {
    var inst = program[pc]
    switch inst {
      case '>':
        ptr++
        if (ptr > 999) { ptr = 0; }
        break;
      case '<':
        ptr--
        if (ptr < 0) { ptr = 999; }
        break;
      case '+':
        mem[ptr]++
        break;
      case '-':
        mem[ptr]--
        break;
      case ',':
        // Ignore
        break;
      case '.':
        println(mem[ptr])
        //output += string(mem[ptr]) DOEST NOT WORK IN golitec
        break;
      case '[':
        if mem[ptr] == 0 {
          counter := 1;
          for counter != 0 {
            pc++
            if program[pc] == '[' { counter++; }
            if program[pc] == ']' { counter--; }
          }
          pc--
        }
        break;
      case ']':
        if mem[ptr] != 0 {
          counter := -1;
          for counter != 0 {
            pc--
            if program[pc] == '[' { counter++; }
            if program[pc] == ']' { counter--; }
          }
        }
        break;
      default:
        stop = true
        break;
    }
    pc++
  }
  return output
}

func main() {
  var prog [1000]rune

  prog[0] = ','
  prog[1] = '+'
  prog[2] = '+'
  prog[3] = '+'
  prog[4] = '+'
  prog[5] = '['
  prog[6] = '>'
  prog[7] = '+'
  prog[8] = '+'
  prog[9] = '+'
  prog[10] = '+'
  prog[11] = '+'
  prog[12] = '<'
  prog[13] = '-'
  prog[14] = ']'
  prog[15] = '>'
  prog[16] = '['
  prog[17] = '<'
  prog[18] = '+'
  prog[19] = '+'
  prog[20] = '+'
  prog[21] = '+'
  prog[22] = '+'
  prog[23] = '>'
  prog[24] = '-'
  prog[25] = ']'
  prog[26] = '+'
  prog[27] = '<'
  prog[28] = '+'
  prog[29] = '['
  prog[30] = '>'
  prog[31] = '['
  prog[32] = '>'
  prog[33] = '+'
  prog[34] = '>'
  prog[35] = '+'
  prog[36] = '<'
  prog[37] = '<'
  prog[38] = '-'
  prog[39] = ']'
  prog[40] = '+'
  prog[41] = '+'
  prog[42] = '>'
  prog[43] = '>'
  prog[44] = '['
  prog[45] = '<'
  prog[46] = '<'
  prog[47] = '+'
  prog[48] = '>'
  prog[49] = '>'
  prog[50] = '-'
  prog[51] = ']'
  prog[52] = '>'
  prog[53] = '>'
  prog[54] = '>'
  prog[55] = '['
  prog[56] = '-'
  prog[57] = ']'
  prog[58] = '+'
  prog[59] = '+'
  prog[60] = '>'
  prog[61] = '['
  prog[62] = '-'
  prog[63] = ']'
  prog[64] = '+'
  prog[65] = '>'
  prog[66] = '>'
  prog[67] = '>'
  prog[68] = '+'
  prog[69] = '['
  prog[70] = '['
  prog[71] = '-'
  prog[72] = ']'
  prog[73] = '+'
  prog[74] = '+'
  prog[75] = '+'
  prog[76] = '+'
  prog[77] = '+'
  prog[78] = '+'
  prog[79] = '>'
  prog[80] = '>'
  prog[81] = '>'
  prog[82] = ']'
  prog[83] = '<'
  prog[84] = '<'
  prog[85] = '<'
  prog[86] = '['
  prog[87] = '['
  prog[88] = '<'
  prog[89] = '+'
  prog[90] = '+'
  prog[91] = '+'
  prog[92] = '+'
  prog[93] = '+'
  prog[94] = '+'
  prog[95] = '+'
  prog[96] = '+'
  prog[97] = '<'
  prog[98] = '+'
  prog[99] = '+'
  prog[100] = '>'
  prog[101] = '>'
  prog[102] = '-'
  prog[103] = ']'
  prog[104] = '+'
  prog[105] = '<'
  prog[106] = '.'
  prog[107] = '<'
  prog[108] = '['
  prog[109] = '>'
  prog[110] = '-'
  prog[111] = '-'
  prog[112] = '-'
  prog[113] = '-'
  prog[114] = '<'
  prog[115] = '-'
  prog[116] = ']'
  prog[117] = '<'
  prog[118] = ']'
  prog[119] = '<'
  prog[120] = '<'
  prog[121] = '['
  prog[122] = '>'
  prog[123] = '>'
  prog[124] = '>'
  prog[125] = '>'
  prog[126] = '>'
  prog[127] = '['
  prog[128] = '>'
  prog[129] = '>'
  prog[130] = '>'
  prog[131] = '['
  prog[132] = '-'
  prog[133] = ']'
  prog[134] = '+'
  prog[135] = '+'
  prog[136] = '+'
  prog[137] = '+'
  prog[138] = '+'
  prog[139] = '+'
  prog[140] = '+'
  prog[141] = '+'
  prog[142] = '+'
  prog[143] = '<'
  prog[144] = '['
  prog[145] = '>'
  prog[146] = '-'
  prog[147] = '<'
  prog[148] = '-'
  prog[149] = ']'
  prog[150] = '+'
  prog[151] = '+'
  prog[152] = '+'
  prog[153] = '+'
  prog[154] = '+'
  prog[155] = '+'
  prog[156] = '+'
  prog[157] = '+'
  prog[158] = '+'
  prog[159] = '>'
  prog[160] = '['
  prog[161] = '-'
  prog[162] = '['
  prog[163] = '<'
  prog[164] = '-'
  prog[165] = '>'
  prog[166] = '-'
  prog[167] = ']'
  prog[168] = '+'
  prog[169] = '['
  prog[170] = '<'
  prog[171] = '<'
  prog[172] = '<'
  prog[173] = ']'
  prog[174] = ']'
  prog[175] = '<'
  prog[176] = '['
  prog[177] = '>'
  prog[178] = '+'
  prog[179] = '<'
  prog[180] = '-'
  prog[181] = ']'
  prog[182] = '>'
  prog[183] = ']'
  prog[184] = '<'
  prog[185] = '<'
  prog[186] = '-'
  prog[187] = ']'
  prog[188] = '<'
  prog[189] = '<'
  prog[190] = '-'
  prog[191] = ']'

  for i:=0; i < 100; i++ {
    println(execute(prog))
  }
}
