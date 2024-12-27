#include "tiger/frame/temp.h"

#include <cstdio>
#include <set>
#include <sstream>

namespace temp {

LabelFactory LabelFactory::label_factory;
TempFactory TempFactory::temp_factory;

/**
 * @brief 检查指定的临时变量 (Temp) 是否在当前 TempList 中
 *
 * 此函数用于判断当前 TempList 是否包含指定的临时变量 (Temp)。
 * 判断依据是比较目标 Temp 对象的编号（通过 `Int()` 方法）是否与列表中的某个
 * Temp 的编号相同。
 *
 * @param t 指向要检查的 Temp 对象的指针。
 * @return bool 如果列表中存在编号与 Temp 对象相同的元素，返回 true；否则返回
 * false。
 *
 * @details
 * - 使用了标准库算法 `std::any_of`，对列表中所有元素进行遍历检查。
 * - 遍历过程中，通过 `Int()` 方法比较目标 Temp 的编号与当前元素的编号。
 * - 如果找到匹配的元素，立即返回 true。
 * - 如果遍历完成仍未找到，返回 false。
 */
bool TempList::Contain(Temp *t) const {
  return std::any_of(temp_list_.cbegin(), temp_list_.cend(),
                     [t](Temp *temp) { return t->Int() == temp->Int(); });
}

/**
 * @brief 检查当前 TempList 是否与另一个 TempList 相等
 *
 * 此函数用于比较当前 TempList 和目标 TempList 是否相等。
 * 两个列表被认为相等的条件是：
 * 1. 两个列表的长度相同。
 * 2. 当前列表中的所有元素都存在于目标列表中。
 * 3. 目标列表中的所有元素都存在于当前列表中。
 *
 * @param tl 指向目标 TempList 的指针。
 * @return bool 如果两个 TempList 相等，返回 true；否则返回 false。
 *
 * @details
 * - 首先检查两个列表是否都为空，如果是，则返回 true。
 * - 如果一个列表为空，而另一个非空，则返回 false。
 * - 使用 `Contain` 方法逐一检查每个列表是否包含对方的所有元素。
 * - 遍历当前列表和目标列表，确保没有遗漏元素。
 */
bool TempList::Equal(TempList *tl) const {
  if (temp_list_.empty() && tl->temp_list_.empty())
    return true;
  if ((!temp_list_.empty() && tl->temp_list_.empty()) ||
      (temp_list_.empty() && !tl->temp_list_.empty()))
    return false;
  for (auto *temp : temp_list_)
    if (!tl->Contain(temp))
      return false;
  for (auto *temp : tl->temp_list_)
    if (!Contain(temp))
      return false;
  return true;
}

/**
 * @brief 替换 TempList 中的一个元素
 *
 * 此函数用于在 TempList 中找到指定的旧元素 (old_temp)，
 * 将其替换为新的元素 (new_temp)。
 *
 * @param old_temp 指向需要被替换的旧 Temp 对象的指针。
 * @param new_temp 指向用于替换的新 Temp 对象的指针。
 *
 * @details
 * - 如果 `old_temp` 或 `new_temp` 是 nullptr，会触发断言失败。
 * - 如果 `old_temp` 不在当前 TempList 中，会触发断言失败。
 * - 遍历 `temp_list_` 找到第一个匹配 `old_temp` 的位置。
 * - 删除旧元素并在相同位置插入新元素。
 */
void TempList::Replace(Temp *old_temp, Temp *new_temp) {
  assert(old_temp && new_temp);
  assert(Contain(old_temp));
  auto it = temp_list_.begin();
  for (; it != temp_list_.end(); it++) {
    if (*it == old_temp) {
      break;
    }
  }
  it = temp_list_.erase(it);
  temp_list_.insert(it, new_temp);
}

/**
 * @brief 从TempList中删除某一个Temp
 *
 * @param t 被删除的Temp
 */
void TempList::Delete(Temp *t) { temp_list_.remove(t); }

/**
 * @brief 计算当前 TempList 和另一个 TempList 的并集
 *
 * 此函数用于计算当前 TempList 与另一个 TempList 的并集，
 * 返回一个新的 TempList 对象，包含两个列表中所有唯一的元素。
 *
 * @param tl 指向另一个 TempList 的指针。
 * @return temp::TempList* 返回一个包含两个列表所有唯一元素的新 TempList 对象。
 *
 * @details
 * - 首先复制当前列表的内容到一个临时列表 `tmp` 中。
 * - 如果目标列表 `tl` 非空且非空列表，将其元素添加到 `tmp` 中。
 * - 使用 `std::set` 来确保结果列表中的元素唯一。
 * - 遍历 `tmp`，将唯一的元素插入到结果列表中。
 *
 * @note
 * - 返回的新 TempList 是动态分配的，调用方需负责释放内存。
 * - 返回的列表保持顺序，即先包含当前列表的顺序，再附加目标列表中不重复的元素。
 */
temp::TempList *TempList::Union(temp::TempList *tl) const {
  temp::TempList tmp = temp::TempList();
  tmp.temp_list_ = std::list<temp::Temp *>(
      temp_list_); //将当前列表的内容复制到 tmp 的内部列表 temp_list_ 中
  if (tl && !tl->temp_list_.empty()) {
    tmp.temp_list_.insert(tmp.temp_list_.end(), tl->temp_list_.begin(),
                          tl->temp_list_.end());
  }
  auto *ret = new temp::TempList();
  std::set<int> temp_set;
  for (auto tmp_temp : tmp.temp_list_) {
    if (temp_set.find(tmp_temp->Int()) != temp_set.end()) {
      continue;
    } else {
      temp_set.insert(tmp_temp->Int());
      ret->temp_list_.push_back(tmp_temp);
    }
  }
  return ret;
}

/**
 * @brief 将另一个 TempList 的内容追加到当前 TempList
 *
 * 此函数用于将目标 TempList (tl) 的所有元素追加到当前 TempList 的末尾。
 * 如果目标列表为空或为 nullptr，则不执行任何操作。
 *
 * @param tl 指向要追加的 TempList 的指针。
 *
 * @details
 * - 检查目标列表 (tl) 是否为 nullptr 或为空列表。
 * - 如果目标列表有效，则将其元素通过迭代器范围 (`begin` 到 `end`)
 * 插入到当前列表的末尾。
 * - 保持当前列表的顺序，同时将目标列表中的所有元素按顺序追加。
 *
 * @note
 * - 此操作不会检查是否存在重复元素。
 * - 目标列表的元素顺序在追加过程中保持不变。
 */
void TempList::CatList(TempList *tl) {
  if (tl && !tl->temp_list_.empty()) {
    temp_list_.insert(temp_list_.end(), tl->temp_list_.begin(),
                      tl->temp_list_.end());
  }
}

/**
 * @brief 计算当前 TempList 与另一个 TempList 的差集
 *
 * 此函数用于计算当前 TempList 和目标 TempList (tl) 的差集，
 * 即返回一个新的 TempList，其中包含当前列表中有但目标列表中没有的元素。
 *
 * @param tl 指向另一个 TempList 的指针。
 * @return temp::TempList* 返回一个包含差集的新 TempList 对象。
 *
 * @details
 * - 如果当前列表为空，直接返回一个空的 TempList。
 * - 如果目标列表为空或为 nullptr，返回当前列表的完整拷贝。
 * - 否则，遍历当前列表中的元素，将那些不在目标列表中的元素添加到结果列表中。
 *
 * @note
 * - 返回的新 TempList 是动态分配的，调用方需负责释放内存。
 * - 不改变当前列表和目标列表的内容。
 */
temp::TempList *TempList::Diff(temp::TempList *tl) const {
  auto *res = new temp::TempList();
  if (temp_list_.empty()) {
    return res;
  } else if (!tl || tl->temp_list_.empty()) {
    res->temp_list_ = std::list<Temp *>(temp_list_);
  } else {
    for (auto temp : temp_list_) {
      if (!tl->Contain(temp)) {
        res->temp_list_.push_back(temp);
      }
    }
  }

  return res;
}

/**
 * @brief 生成一个名称为 `L{label_id_++}` 的label
 *
 * @return Label*
 */
Label *LabelFactory::NewLabel() {
  char buf[100];
  sprintf(buf, "L%d", label_factory.label_id_++);
  return NamedLabel(std::string(buf));
}

/**
 * Get symbol of a label_. The label_ will be created only if it is not found.
 * @param s label_ string
 * @return symbol
 */
Label *LabelFactory::NamedLabel(std::string_view s) {
  return sym::Symbol::UniqueSymbol(s);
}

/**
 * @brief 得到Label的Name
 *
 * @param s 传入的Label
 * @return std::string Label的Name
 */
std::string LabelFactory::LabelString(Label *s) { return s->Name(); }

/**
 * @brief 创建一个新的临时变量（Temp 对象）
 *
 * 此函数是 TempFactory 类的核心方法，用于动态生成唯一的临时变量。
 * 每次调用会创建一个新的 Temp 对象，并为其分配一个唯一的编号。
 * 同时，将该临时变量与对应的字符串标识（如 "t0", "t1"）存储到 Map 中，
 * 方便后续查找或调试。
 *
 * @return Temp* 返回指向新创建的 Temp 对象的指针。
 */
Temp *TempFactory::NewTemp() {
  Temp *p = new Temp(temp_factory.temp_id_++);
  std::stringstream stream;
  stream << 't';
  stream << p->num_;
  Map::Name()->Enter(p, new std::string(stream.str()));

  return p;
}

int Temp::Int() const { return num_; }

/**
 * @brief 创建一个空的 Map 对象
 *
 * 此函数用于生成一个新的、没有任何映射关系的 Map 实例。
 * 返回的 Map 对象内部的映射表（tab_）为空，且不关联任何下层 Map。
 *
 * @return Map* 指向新创建的空 Map 对象的指针。
 */
Map *Map::Empty() { return new Map(); }

/**
 * @brief 获取一个全局共享的 Map 对象实例
 *
 * 此函数返回一个静态的 Map 实例，用于存储临时变量（Temp）
 * 到其字符串标识的映射关系（如 "t0" 对应到某个 Temp 对象）。
 * 通过静态变量实现单例模式，确保全局只有一个这样的 Map 实例。
 *
 * - 如果该实例尚未创建，则调用 Map::Empty() 初始化。
 * - 如果该实例已经存在，则直接返回。
 *
 * @return Map* 指向全局共享 Map 对象的指针。
 */
Map *Map::Name() {
  static Map *m = nullptr;
  if (!m)
    m = Empty();
  return m;
}

/**
 * @brief 创建一个多层 Map 映射
 *
 * 此函数将两个 Map 对象层叠起来，形成一个多层映射的结构。
 * 这种多层结构允许在上层（over）未找到键值时，递归到下层（under）进行查找。
 *
 * @param over 指向上层 Map 的指针，可以为 nullptr 表示不存在上层。
 * @param under 指向下层 Map 的指针，可以为 nullptr 表示不存在下层。
 * @return Map* 一个新的 Map 实例，其内容为将 over 叠加到 under 之上。
 *
 * @details
 * - 如果 `over` 为 nullptr，直接返回 `under`，表示没有需要叠加的上层。
 * - 如果 `over` 存在，则：
 *   - 创建一个新的 Map。
 *   - 将 `over` 的映射表（`over->tab_`）作为新 Map 的当前层。
 *   - 将 `over->under_` 与 `under` 层叠作为新 Map 的下层。
 */
Map *Map::LayerMap(Map *over, Map *under) {
  if (over == nullptr)
    return under;
  else
    return new Map(over->tab_, LayerMap(over->under_, under));
}

/**
 * @brief 在 Map 中插入或更新一个键值对
 *
 * 此函数将一个临时变量（Temp）与一个字符串（string）建立映射关系，
 * 并将其存储到当前 Map 的映射表（tab_）中。
 *
 * @param t 指向要映射的 Temp 对象的指针。
 * @param s 指向对应字符串的指针。
 *
 * @details
 * - 如果 `tab_` 不为 nullptr，则将键值对插入或更新到当前映射表。
 * - 使用断言 (`assert`) 确保 `tab_` 已被正确初始化。
 */
void Map::Enter(Temp *t, std::string *s) {
  assert(tab_);
  tab_->Enter(t, s);
}

/**
 * @brief 在 Map 中查找与指定临时变量 (Temp) 关联的字符串
 *
 * 此函数用于在当前 Map 和可能的下层 Map 中递归查找指定的键（Temp 对象），
 * 并返回其对应的字符串值。如果当前层和所有下层都找不到，则返回 nullptr。
 *
 * @param t 指向要查找的 Temp 对象的指针。
 * @return std::string* 指向与 Temp 关联的字符串的指针；如果未找到，则返回
 * nullptr。
 *
 * @details
 * - 首先在当前 Map 的映射表（`tab_`）中查找 Temp 对象。
 * - 如果找到，直接返回对应的字符串。
 * - 如果当前层未找到，并且存在下层 Map（`under_` 非 nullptr），递归到下层查找。
 * - 如果所有层次都未找到，返回 nullptr。
 *
 * @note 此函数使用了断言 (`assert(tab_)`) 确保当前层的映射表已被正确初始化。
 */
std::string *Map::Look(Temp *t) {
  std::string *s;
  assert(tab_);
  s = tab_->Look(t);
  if (s)
    return s;
  else if (under_)
    return under_->Look(t);
  else
    return nullptr;
}

/**
 * @brief 打印 Map 的内容到指定的文件流
 *
 * 此函数将当前 Map 中的键值对（Temp
 * 到字符串的映射关系）以格式化形式打印到指定的文件流中， 并递归打印下层 Map
 * 的内容（如果存在）。
 *
 * @param out 指向目标文件流的指针，用于输出内容（例如 stdout 或文件）。
 *
 * @details
 * - 使用 `tab_->Dump` 遍历当前 Map
 * 的映射表，并为每个键值对调用回调函数进行格式化输出。
 * - 如果当前 Map 存在下层 Map（`under_` 非空），递归调用下层 Map 的 `DumpMap`。
 * - 每个层次的内容之间使用分割线 (`"---------"`) 分隔，便于识别层次结构。
 *
 * @note
 * - 必须确保 `tab_` 已正确初始化，否则可能导致未定义行为。
 * - 目标文件流 `out` 应该是有效的，例如已打开的文件或 `stdout`。
 */
void Map::DumpMap(FILE *out) {
  tab_->Dump([out](temp::Temp *t, std::string *r) {
    fprintf(out, "t%d -> %s\n", t->Int(), r->data());
  });
  if (under_) {
    fprintf(out, "---------\n");
    under_->DumpMap(out);
  }
}

} // namespace temp