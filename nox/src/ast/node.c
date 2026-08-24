#include "node.h"

#include <stdio.h>
#include <string.h>

/// Number of hash buckets
///
/// This is a prime number, as N % prime producess less collisions.
/// While we do not know what is the most optimal prime number to use,
/// but if we expect that there'll be max 65,536 unique names in a module,
/// then the average number of elements in each bucket would be,
///
///     Load Factor: 65,536/16001 = 4.0;
///
/// That seems good enough for us.
const usize NUM_HASHNAME_BUCKETS = 16001;

/// Number of unique names that can exist in each module.
/// We assume that it will be 50,000
const usize NUM_NAME_BUCKETS = 2;

void initModule(Allocator * mem, ModuleNode * mod) {
    mod->hashNames = memoryAlloc(mem, NUM_HASHNAME_BUCKETS, sizeof(HashNameList), 16);
    mod->names = memoryAlloc(mem, NUM_NAME_BUCKETS * 64, sizeof(char), 16);
    mod->maxNumNames = NUM_NAME_BUCKETS;
    mod->numNames = 0;

    for (usize i = 0; i < NUM_HASHNAME_BUCKETS; i++) {
        mod->hashNames[i] = (HashNameList){0};
        mod->hashNames[i].len = 0;
        mod->hashNames[i].cap = 8;
    }

    memset(mod->names, '\0', NUM_NAME_BUCKETS * 64);
}

usize findNameHash(const ModuleNode * mod, const char * str, u32 hash, usize bucketIndex, usize strLen) {
    for (usize i = 0; i < mod->hashNames[bucketIndex].len; i++) {
        if (mod->hashNames[bucketIndex].names[i].hash == hash) {
            if (stringEqual(mod->hashNames[bucketIndex].names[i].str, str, strLen)) {
                return i;
            }
        }
    }
    return UINT32_MAX;
}

usize storeNameHash(ModuleNode * mod, const char * str, usize strLen) {
    const u32 hash = fnv1a(str, strLen);
    const usize bucketIndex = hash % NUM_HASHNAME_BUCKETS;
    const usize nameHashIndex = findNameHash(mod, str, hash, bucketIndex, strLen);

    usize id = UINT32_MAX;

    if (nameHashIndex == UINT32_MAX) {
        const usize nameIndex = at(mod->hashNames[bucketIndex].len, mod->hashNames[bucketIndex].cap);
        mod->hashNames[bucketIndex].names[nameIndex] = (HashName){
            .hash = hash,
            .len = (u8)strLen,
            .str = (char *)str,
        };
        mod->hashNames[bucketIndex].len++;
        id = bucketIndex * 8 + nameIndex;
    } else {
        id = bucketIndex * 8 + nameHashIndex;
    }

    return id;
}
