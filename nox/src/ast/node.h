#pragma once
#include "../base.h"

typedef struct TypeNode {
    u32 selfIndex;
} TypeNode;

typedef struct ExprNode {
    u32 selfIndex;

    enum {
        EXPR_BLOCK = 1,
        EXPR_TUPLE,
        EXPR_NICK,
        EXPR_NAME,
        EXPR_XINT,
        EXPR_UNIT,
    } kind;

    union {
        union {
            u32 stmtSetIndex;
        } block;

        union {
            u32 exprSetIndex;
        } tuple;

        union {
            u32 nameIndex;
        } name;

        union {
            usize bsize;
            u64 b64;
            u32 b32;
        } xint;
    } data;
} ExprNode;

typedef struct StmtNode {
    u32 selfIndex;
    enum {
        STMT_LET,
        STMT_RETURN,
        STMT_YIELD,
    } kind;
    u32 exprIndex;
} StmtNode;

typedef struct EntityNode {
    u32 selfIndex;
    UUID uuid;

    enum {
        ENTITY_MODULE = 1,
        ENTITY_FN,
    } kind;

    union {
        //
        // mod math.{add, div} as m;
        //     + (name)           + (alias)
        //           +-------+ (imports)
        //
        struct {
            u32 nameIndex;
            u32x2 importRange;
            u32 aliasIndex;
            bool hasImports;
            bool hasAlias;
        } mod;

        //
        // export fn add(x int, y int) int { /* block */ }
        // +(export) +(name)           +(type) +(block)
        //               +----------+ (args)
        //
        struct {
            bool hasExport;
            u32 nameIndex;
            u32x2 argsRange;
            u32 typeIndex;
            u32 blockIndex;
        } fn;
    } data;

    StmtNode * stmts;
    ExprNode * exprs;

    u32 numStmts;
    u32 numExprs;
} EntityNode;

typedef struct FileNode {
    /// Index to this file in an array of files
    u32 selfIndex;
    UUID uuid;

    /// Store the filepath so we can debug bugs.
    const char * filePath;

    /// Entities stored in the file
    EntityNode * entys;
    StmtNode * stmts;
    ExprNode * exprs;

    u32 numEntys;
    u32 numStmts;
    u32 numExprs;

    u32 maxNumEntys;
    u32 maxNumStmts;
    u32 maxNumExprs;
} FileNode;

typedef struct HashName {
    u32 hash;
    u32 len;
    char * str;
} HashName;

typedef struct HashNameList {
    HashName names[8];
    u8 len;
    u8 cap;
} HashNameList;

typedef struct Module {
    u32 selfIndex;
    UUID uuid;

    /// Files stored in the module
    FileNode * files;
    u32 numFiles;

    /// Hash table of names stored in the module
    HashNameList * hashNames;
    /// Names stored in the module
    void * names;
    u32 maxNumNames;
    u32 numNames;

    /// Types stored in the module
    TypeNode * types;
    u32 numTypes;
} ModuleNode;

void initModule(Allocator * mem, ModuleNode * m);
usize storeNameHash(ModuleNode * mod, const char * str, usize strLen);
