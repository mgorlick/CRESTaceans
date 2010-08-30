#include <allegro5/allegro.h>

ALLEGRO_KEYBOARD_STATE* get_keyboard_state(void) {

  ALLEGRO_KEYBOARD_STATE *ret_state = malloc(sizeof(ALLEGRO_KEYBOARD_STATE));
  al_get_keyboard_state(ret_state);
  return ret_state;
  
}

void delete_keyboard_state(ALLEGRO_KEYBOARD_STATE *ret_state) {
  free(ret_state);
}

int main(void) {

  return 0;
}
