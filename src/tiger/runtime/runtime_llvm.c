#include <stdio.h>
#include <stdlib.h>

extern int tigermain(long long rsp, long long sl);

// seven arguments testcase
// 取七个参数之和
int sum_seven(int v1, int v2, int v3, int v4, int v5, int v6, int v7) {
  return v1 + v2 + v3 + v4 + v5 + v6 + v7;
}

/**
 * @brief 初始化一个数组
 *
 * @param size 数组的大小
 * @param init 数组的初始值
 * @return long long 初始化成功的数组指针
 */
long long init_array(int size, long init) {
  int i;
  long *a = (long *)malloc(size * sizeof(long));
  for (i = 0; i < size; i++)
    a[i] = init;
  return (long long)a;
}

/**
 * @brief malloc一个size大小的数组，初始值为0
 *
 * @param size 数组的大小
 * @return long long 创建成果的数组
 */
long long alloc_record(int size) {
  int i;
  int *p, *a;
  p = a = (int *)malloc(size);
  for (i = 0; i < size; i += sizeof(int))
    *p++ = 0;
  return (long long)a;
}

struct LLVMString {
  int length;      // length of string
  char padding[4]; // padding for alignment
  char *chars;     // actual string content
};

/**
 * @brief 判断两个 LLVMString 是否相等（包括长度以及内容）。
 *
 * @param s 指向第一个 LLVMString 的指针。
 * @param t 指向第二个 LLVMString 的指针。
 * @return int 如果相等，返回 1；如果不相等，返回 0。
 */
int string_equal(struct LLVMString *s, struct LLVMString *t) {
  int i;
  if (s == t)
    return 1;
  if (s->length != t->length)
    return 0;
  for (i = 0; i < s->length; i++)
    if (s->chars[i] != t->chars[i])
      return 0;
  return 1;
}

/**
 * @brief 打印一个 LLVMString 的内容到标准输出。
 *
 * @param s 指向 `LLVMString` 结构体的指针，其中包含要打印的字符串数据和其长度。
 */
void print(struct LLVMString *s) {
  int i;
  char *p = s->chars;
  for (i = 0; i < s->length; i++, p++)
    putchar(*p);
}

/**
 * @brief 打印一个整数到标准输出。
 *
 * @param k 要打印的整数。
 */
void printi(int k) { printf("%d", k); }

/**
 * @brief 刷新标准输出缓冲区，将缓冲区内容立即输出到终端。
 *
 * @note 通常用于确保标准输出缓冲区中的数据在需要时立即输出到显示设备。
 */
void flush() { fflush(stdout); }

/**
 * @brief const[i]存储着第i个ASCII码
 *
 */
struct LLVMString consts[256];
struct LLVMString empty = {
    .length = 0,    // 初始化 value
    .padding = {0}, // 初始化 padding，默认全为 0
    .chars = ""     // 将字符串常量的地址赋值给 ptr
};

int main() {
  int i;
  for (i = 0; i < 256; i++) {
    consts[i].length = 1;
    consts[i].chars = (char *)malloc(1);
    consts[i].chars[0] = i;
  }
  // 动态分配 0x100000（1 MB）的内存，作为运行时的栈空间。
  long long stack_bottom = (long long)malloc(0x100000);
  long long aligned_address = (stack_bottom + 0x100000) & ~0xFFFF;
  return tigermain(aligned_address, aligned_address);
}

/**
 * @brief 返回给定 LLVMString 的第一个字符的 ASCII 值。
 *
 * @param s 指向 LLVMString 结构体的指针，表示需要处理的字符串。
 *          其中 `s->chars` 是字符数组，`s->length` 是字符串长度。
 * @return int 如果字符串非空，返回第一个字符的 ASCII 值；如果字符串为空，返回
 * -1。
 */
int ord(struct LLVMString *s) {
  if (s->length == 0)
    return -1;
  else
    return s->chars[0];
}

/**
 * @brief 返回一个指向包含指定字符的 LLVMString 的指针。
 *
 * @param i 输入整数，表示要获取的字符的 ASCII 值。有效范围为 [0, 255]。
 * @return struct LLVMString* 指向包含指定字符的 LLVMString 的指针。
 *
 * @note
 * - 如果输入的整数超出范围（< 0 或 >= 256），函数会打印错误信息并终止程序。
 * - 返回的指针指向全局 `consts` 数组中的对应元素。
 *
 * @example
 * struct LLVMString *str = chr(65); // 返回包含字符 'A' 的 LLVMString。
 * printf("%c\n", str->chars[0]);    // 输出: A
 *
 * @error
 * - 当输入超出范围时，打印错误信息并调用 `exit(1)` 终止程序。
 */
struct LLVMString *chr(int i) {
  if (i < 0 || i >= 256) {
    printf("chr(%d) out of range\n", i);
    exit(1);
  }
  return consts + i;
}

/**
 * @brief 获取给定 LLVMString 的长度。
 *
 * @param s 指向 LLVMString 结构体的指针，用于表示目标字符串。
 *          其中 `s->length` 表示字符串的长度。
 * @return int 返回字符串的长度。
 *         如果指针 `s` 为 NULL，可能导致未定义行为。
 */
int size(struct LLVMString *s) { return s->length; }

/**
 * @brief 从给定的 LLVMString 中提取子字符串。
 *
 * @param s 指向 LLVMString 结构体的指针，表示源字符串。
 * @param first 子字符串起始位置（基于 0 的索引）。
 * @param n 子字符串的长度。
 * @return struct LLVMString* 返回一个指向新创建的 LLVMString
 * 的指针，表示提取的子字符串。 如果 n 为 1，直接返回指向全局 `consts`
 * 数组的元素；否则返回一个动态分配的字符串。
 *
 * @note
 * - 如果 `first` 或 `n` 越界，函数会打印错误信息并终止程序。
 * - 返回的 LLVMString 需要由调用者手动释放其内存（`free()`）。
 *
 * @error
 * - 如果 `first` 或 `first + n` 超出字符串范围，会打印错误信息并调用
 * `exit(1)`。
 */
struct LLVMString *substring(struct LLVMString *s, int first, int n) {
  if (first < 0 || first + n > s->length) {
    printf("substring([%d],%d,%d) out of range\n", s->length, first, n);
    exit(1);
  }
  if (n == 1)
    return consts + s->chars[first];
  {
    struct LLVMString *t =
        (struct LLVMString *)malloc(sizeof(struct LLVMString));
    t->chars = (char *)malloc(n);
    int i;
    t->length = n;
    for (i = 0; i < n; i++)
      t->chars[i] = s->chars[first + i];
    return t;
  }
}

/**
 * @brief 连接两个 LLVMString 字符串并返回一个新LLVMString。
 *
 * @param a 指向第一个 LLVMString 的指针。
 * @param b 指向第二个 LLVMString 的指针。
 * @return struct LLVMString*
 * - 如果 `a` 的长度为 0，直接返回 `b`。
 * - 如果 `b` 的长度为 0，直接返回 `a`。
 * - 如果 `a` 和 `b` 都非空，返回一个动态分配的 LLVMString，包含连接后的结果。
 *
 * @note 返回的新字符串需要调用者负责释放内存（包括 `chars` 和 `LLVMString`
 * 本身）。
 *
 * @error
 * - 如果 `a` 或 `b` 为 NULL，可能导致未定义行为。
 */
struct LLVMString *concat(struct LLVMString *a, struct LLVMString *b) {
  if (a->length == 0)
    return b;
  else if (b->length == 0)
    return a;
  else {
    int i, n = a->length + b->length;
    struct LLVMString *t =
        (struct LLVMString *)malloc(sizeof(struct LLVMString));
    t->chars = (char *)malloc(n);
    t->length = n;
    for (i = 0; i < a->length; i++)
      t->chars[i] = a->chars[i];
    for (i = 0; i < b->length; i++)
      t->chars[i + a->length] = b->chars[i];
    return t;
  }
}

/**
 * @brief 返回！i
 *
 * @param i
 * @return int
 */
int get_not(int i) { return !i; }

#undef getchar

/**
 * @brief 从标准输入获取一个字符并返回对应的 LLVMString 指针。
 *
 * @return struct LLVMString*
 * - 如果读取到字符，则返回指向对应字符的 LLVMString。
 * - 如果遇到文件结束符（EOF），返回全局变量 `empty` 的地址。
 *
 * @note
 * - 函数使用 `getc(stdin)` 从标准输入读取字符。
 * - 返回的指针不是动态分配的，因此调用者不需要释放内存。
 * - 全局变量 `consts` 应包含所有可能的 ASCII 字符（256 个 `LLVMString`）。
 *
 */
struct LLVMString *__wrap_getchar() {
  int i = getc(stdin);
  if (i == EOF)
    return &empty;
  else
    return consts + i;
}