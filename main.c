#include <stdbool.h>
#include <stdint.h>
#include <avr/interrupt.h>
#include <util/delay.h>

#include "cadet.h"
#include "usb.h"
#include "print.h"

int main(void);
uint16_t read_column(void);
void deselect_row(uint8_t row);
void select_row(uint8_t row);

void select_row(uint8_t row) {
    if (row == 0) {
        DDRB |= 1 << 7;
    } else if (row == 1) {
        DDRB |= 1 << 3;
    } else if (row == 2) {
        DDRB |= 1 << 2;
    } else if (row == 3) {
        DDRB |= 1 << 1;
    } else if (row == 4) {
        DDRB |= 1 << 0;
    }
}

void deselect_row(uint8_t row) {
    if (row == 0) {
        DDRB &= ~(1 << 7);
    } else if (row == 1) {
        DDRB &= ~(1 << 3);
    } else if (row == 2) {
        DDRB &= ~(1 << 2);
    } else if (row == 3) {
        DDRB &= ~(1 << 1);
    } else if (row == 4) {
        DDRB &= ~(1 << 0);
    }
}

uint16_t read_column(void) {
    return (PIND & (1 << 0) ? 0 : (1 << 0)) |
        (PIND & (1 << 1) ? 0 : (1 << 1)) |
        (PIND & (1 << 2) ? 0 : (1 << 2)) |
        (PIND & (1 << 3) ? 0 : (1 << 3)) |
        (PIND & (1 << 5) ? 0 : (1 << 4)) |
        (PIND & (1 << 4) ? 0 : (1 << 5)) |
        (PIND & (1 << 6) ? 0 : (1 << 6)) |
        (PIND & (1 << 7) ? 0 : (1 << 7)) |
        (PINB & (1 << 4) ? 0 : (1 << 8)) |
        (PINB & (1 << 5) ? 0 : (1 << 9)) |
        (PINB & (1 << 6) ? 0 : (1 << 10)) |
        (PINC & (1 << 6) ? 0 : (1 << 11)) |
        (PINC & (1 << 7) ? 0 : (1 << 12)) |
        (PINE & (1 << 6) ? 0 : (1 << 13)) |
        (PINF & (1 << 1) ? 0 : (1 << 14));
}

void print_matrix(uint16_t matrix[]) {
    for (uint8_t i = 0; i < 5; i++) {
        for (uint8_t j = 0; j < 15; j++) {
            if ((matrix[i] >> j) & 1) {
                print("1");
            } else {
                print("0");
            }
        }
        print("\n");
    }
}

uint8_t keymap[2][5][15] = {
    {
        {KEY_ESC, KEY_1, KEY_2, KEY_3, KEY_4, KEY_5, KEY_6, KEY_7, KEY_8, KEY_9, KEY_0, KEY_MINUS, KEY_EQUAL, KEY_BACKSLASH, KEY_TILDE},
        {KEY_TAB, KEY_Q, KEY_W, KEY_E, KEY_R, KEY_T, KEY_Y, KEY_U, KEY_I, KEY_O, KEY_P, KEY_LEFT_BRACE, KEY_RIGHT_BRACE, KEY_BACKSPACE, 0},
        {KEY_LEFT_CTRL, KEY_A, KEY_S, KEY_D, KEY_F, KEY_G, KEY_H, KEY_J, KEY_K, KEY_L, KEY_SEMICOLON, KEY_QUOTE, 0, KEY_ENTER, 0},
        {KEY_LEFT_SHIFT, 0, KEY_Z, KEY_X, KEY_C, KEY_V, KEY_B, KEY_N, KEY_M, KEY_COMMA, KEY_PERIOD, KEY_SLASH, 0, KEY_RIGHT_SHIFT, KEY_FN},
        {KEY_CAPS_LOCK, KEY_LEFT_GUI, KEY_LEFT_ALT, 0, 0, 0, KEY_SPACE, 0, 0, 0, KEY_RIGHT_ALT, KEY_RIGHT_GUI, KEY_APP, KEY_RIGHT_CTRL, 0}
    },
    {
        {0, KEY_F1, KEY_F2, KEY_F3, KEY_F4, KEY_F5, KEY_F6, KEY_F7, KEY_F8, KEY_F9, KEY_F10, KEY_F11, KEY_F12, KEY_INSERT, KEY_DELETE},
        {0, 0, 0, 0, 0, 0, 0, 0, KEY_PRINTSCREEN, KEY_SCROLL_LOCK, KEY_PAUSE, KEY_UP, 0, 0, 0},
        {0, KEY_VOLUME_DOWN, KEY_VOLUME_UP, KEY_MUTE, 0, 0, KEYPAD_ASTERIX, KEYPAD_SLASH, KEY_HOME, KEY_PAGE_UP, KEY_LEFT, KEY_RIGHT, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, KEYPAD_PLUS, KEYPAD_MINUS, KEY_END, KEY_PAGE_DOWN, KEY_DOWN, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    },
};

int main(void) {
    // set for 16 MHz clock
    CLKPR = 0x80;
    CLKPR = 0;

    // Initialize the USB, and then wait for the host to set configuration.
    // If the Teensy is powered without a PC connected to the USB port,
    // this will wait forever.
    usb_init();
    while (!usb_configured()) /* wait */ ;

    // Wait an extra second for the PC's operating system to load drivers
    // and do whatever it does to actually be ready for input
    _delay_ms(1000);

    //       0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
    //      D0 D1 D2 D3 D5 D4 D6 D7 B4 B5 B6 C6 C7 E6 F1
    // 0 B7
    // 1 B3
    // 2 B2
    // 3 B1
    // 4 B0

    // set pin mode
    PORTB = 0b01110000;
    PORTC = 0b11111111;
    PORTD = 0b11111111;
    PORTE = 0b11111111;
    PORTF = 0b11111111;

    uint16_t matrix_c[5] = {0, 0, 0, 0, 0};
    uint16_t matrix_p[5] = {0, 0, 0, 0, 0};
    uint16_t matrix_d[5] = {0, 0, 0, 0, 0};

    uint8_t debouncing = 5;

    print("Ivory Cadet online.\n");
    for (;;) {
        // read and debounce matrix
        for (uint8_t i = 0; i < 5; i++) {
            select_row(i);
            _delay_us(50);
            uint16_t column = read_column();
            if (matrix_d[i] != column) {
                matrix_d[i] = column;
                if (debouncing) {
                    print("bounce!: "); phex(debouncing); print("\n");
                }
                debouncing = 5;
            }
            deselect_row(i);
        }
        if (debouncing) {
            debouncing--;
            if (debouncing) {
                _delay_ms(1);
            } else {
                for (uint8_t i = 0; i < 5; i++) {
                    matrix_c[i] = matrix_d[i];
                }
            }
        }
        // compare current and previous matrix
        if (matrix_changed(matrix_p, matrix_c) == true) {
            update_report(matrix_c, keymap);
            keyboard_modifier_keys = report.modifier;
            memcpy(&keyboard_keys, &report.keycode, sizeof keyboard_keys);
            usb_keyboard_send();
        }
        // set previous matrix
        for (uint8_t i = 0; i < 5; i++) {
            matrix_p[i] = matrix_c[i];
        }
    }
}
