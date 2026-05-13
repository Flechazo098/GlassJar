# GlassJar

GlassJar 是一个用于**对比 JAR 文件差异**的命令行工具。它不仅能检测文件的增删改，还能将发生变化的 `.class` 文件反编译为 Java 源码，直观展示代码层面的变更。支持四种输出格式：终端彩色 diff、纯文本、JSON 和交互式 HTML。

## 安装

### 前置依赖

- [GHC 9.6+](https://www.haskell.org/ghc/) 和 [Cabal](https://www.haskell.org/cabal/)（Haskell 编译工具链）
- [JDK](https://adoptium.net/)（反编译需要运行 CFR/Vineflower）

### 编译

```bash
cabal update
cabal build
```

编译完成后，可执行文件位于 `dist-newstyle` 目录下。

## 使用方法

```bash
glassjar <旧文件> <新文件> [选项]
```

两个输入可以是 JAR、ZIP、目录或单个文件。

### 基本示例

```bash
# 对比两个 JAR 包，终端彩色输出
glassjar old.jar new.jar

# 输出为 HTML 报告
glassjar old.jar new.jar -f html

# 输出为 JSON 格式
glassjar old.jar new.jar -f json

# 对比两个目录
glassjar ./v1/ ./v2/
```

### 常用选项

| 选项 | 说明 | 默认值 |
|------|------|--------|
| `-f, --format` | 输出格式：`gitdiff`、`text`、`json`、`html` | `gitdiff` |
| `--decompiler` | 反编译器：`auto`、`cfr`、`vineflower` | `auto` |
| `--decompile-jobs` | 并行反编译线程数 | `4` |
| `--hide-lambda` | 隐藏反编译结果中的 lambda 行 | 关闭 |
| `--ignore-class-same-size` | 跳过未压缩大小相同的 class 文件 | 关闭 |
| `--disable-inner-group` | 不将内部类归组到外部类 | 关闭 |
| `--disable-decompile-cache` | 禁用反编译缓存 | 关闭 |

### HTML 报告功能

生成的 HTML 报告是一个自包含的单文件，支持：
- 分割视图 / 统一视图切换
- 暗色 / 亮色主题切换
- 可按 `entry-N` 参数聚焦到特定条目
- 代码语法高亮

### 退出码

- `0` — 未发现差异
- `1` — 发现差异或读取错误
