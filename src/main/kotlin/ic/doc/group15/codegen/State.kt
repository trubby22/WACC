package ic.doc.group15.codegen

import java.util.*
import kotlin.collections.HashMap

class State {
    // to store the stack positions of identifiers, we maintain a Stack<INT> for each identifier name.
    // the reason we store a Stack<INT> of stack positions rather than simply the stack position alone
    // is for the case of reusing a variable identifier within a more local scope. when this happens,
    // the most local variables stack position is pushed to the top of the Stack<INT>. whenever a scope
    // is exited, we pop all of the relevant stacks.

    val map : HashMap<String, Stack<Int>> = hashMapOf()

    fun setStackPos(ident : String, stackPos : Int) {
        if (!map.containsKey(ident)) {
            map[ident] = Stack()
            (map[ident] as Stack).push(stackPos)
        } else {
            (map[ident] as Stack).push(stackPos)
        }
    }

    fun popStacks(idents : List<String>) {
        for (ident in idents) {
            (map[ident] as Stack).pop()
        }
    }

    fun getStackPos(ident: String) : Int {
        return (map[ident] as Stack).peek()
    }
}