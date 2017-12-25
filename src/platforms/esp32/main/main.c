/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "freertos/FreeRTOS.h"
#include "esp_system.h"
#include "esp_event.h"
#include "esp_event_loop.h"
#include "nvs_flash.h"

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "globalcontext.h"
#include "iff.h"
#include "mapped_file.h"
#include "module.h"
#include "utils.h"
#include "term.h"

esp_err_t event_handler(void *ctx, system_event_t *event)
{
    return ESP_OK;
}

extern const unsigned char beam_file[624];

void app_main()
{
    nvs_flash_init();
    tcpip_adapter_init();
    ESP_ERROR_CHECK(esp_event_loop_init(event_handler, NULL));

    Module *mod = module_new_from_iff_binary(beam_file, 624);
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    printf("Execute\n");
    context_execute_loop(ctx, mod, beam_file, "start", 0);
    printf("Return value: %lx\n", (long) term_to_int32(ctx->x[0]));

    while(1);
}
