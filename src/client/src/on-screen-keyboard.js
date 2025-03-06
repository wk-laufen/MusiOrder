import Keyboard from 'simple-keyboard'

const textLayout = {
    default: [
        "^ 1 2 3 4 5 6 7 8 9 0 \u00DF \u00B4 {bksp}",
        "{tab} q w e r t z u i o p \u00FC +",
        "{lock} a s d f g h j k l \u00F6 \u00E4 # {enter}",
        "{shift} < y x c v b n m , . - {shift}",
        "@ {space}",
    ],
    shift: [
        '\u00B0 ! " \u00A7 $ % & / ( ) = ? ` {bksp}',
        "{tab} Q W E R T Z U I O P \u00DC *",
        "{lock} A S D F G H J K L \u00D6 \u00C4 ' {enter}",
        "{shift} > Y X C V B N M ; : _ {shift}",
        "@ {space}",
    ],
}
// see https://stackoverflow.com/a/51411310/1293659
const decimalPoint = new Intl.NumberFormat(navigator.language).formatToParts(1.1).find(part => part.type === 'decimal').value
const numpadLayout = {
    default: [
        `1 2 3 4 5 6 7 8 9 0 ${decimalPoint}`,
        '{bksp} {enter}',
    ]
}

let activeKeyboard = undefined

function inputChangedHandler(event) {
    if (activeKeyboard === undefined) {
        return
    }
    activeKeyboard.keyboard.setInput(event.target.value)
}

const destroyActiveKeyboard = () => {
    activeKeyboard.keyboard.destroy()
    activeKeyboard.inputElement.removeEventListener('change', inputChangedHandler)
    activeKeyboard = undefined
}

// see https://stackoverflow.com/a/64877531/1293659
const setValue = (inputElement, value) => {
    const setter = Object.getOwnPropertyDescriptor(inputElement, 'value').set
    const prototype = Object.getPrototypeOf(inputElement)
    const prototypeValueSetter = Object.getOwnPropertyDescriptor(prototype, 'value').set
    if (setter && setter !== prototypeValueSetter) {
        prototypeValueSetter.call(inputElement, value)
    } else {
        setter.call(inputElement, value)
    }
    inputElement.dispatchEvent(new Event('input', { 'bubbles': true }))
}

const showKeyboard = inputElement => {
    if (activeKeyboard !== undefined) {
        if (activeKeyboard.inputElement === inputElement) {
            return
        }
        else {
            destroyActiveKeyboard()
        }
    }

    const layout = inputElement.inputMode === 'decimal' ? numpadLayout : textLayout

    const keyboard = new Keyboard({
        layout,
        display: {
            '{bksp}': '⌫',
            '{enter}': '↵',
            '{shift}': '⇧',
            '{lock}': '⇪',
            '{tab}': '↹'
        },
        mergeDisplay: true,
        onChange: input => {
            setValue(inputElement, input)
        },
        onKeyPress: button => {
            if (button === '{shift}' || button === '{lock}') {
                keyboard.setOptions({
                    layoutName: keyboard.options.layoutName === 'default' ? 'shift' : 'default'
                })
            }
        }
    })
    keyboard.setInput(inputElement.value)

    activeKeyboard = { keyboard, inputElement }

    inputElement.addEventListener('input', inputChangedHandler)
}

const isInputElement = element => {
    return element.tagName.toLowerCase() === 'input' &&
        element.type.toLowerCase() === 'text'
}

export const setupKeyboards = () => {
    document.body.addEventListener('focus', event => {
        if(isInputElement(event.target)) {
            showKeyboard(event.target)
        }
    }, true)

    function destroyHandler(event) {
        if (activeKeyboard === undefined) {
            return
        }
        if (isInputElement(event.target) ||
            event.target.className.includes('hg-button') ||
            event.target.className.includes('hg-row') ||
            event.target.className.includes('simple-keyboard')) {
            return
        }
        destroyActiveKeyboard()
    }
    document.addEventListener('click', destroyHandler, true)
}
