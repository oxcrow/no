# Demonstrate use of an aggregate type
type :one = { w }

# Initalize an object's value
function $initOne(l %x) {
@start
    storew 1, %x
    ret
}

# Return an object's value
function w $getOne(l %x) {
@start
    %value = w loadw %x
    ret %value
}

export function w $main() {
@start
    %x = l alloc4 4                 # Allocate an object of 4 bytes, alligned to 4 bytes
    call $initOne(l %x)             # Initialize the object's value
    %y = w call $getOne(l %x)       # Get the object's value
    ret %y
}
