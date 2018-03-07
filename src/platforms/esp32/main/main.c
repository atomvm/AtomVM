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
#include "esp_partition.h"
#include "esp_system.h"
#include "esp_event.h"
#include "esp_event_loop.h"
#include "nvs_flash.h"

#include "atom.h"
#include "avmpack.h"
#include "bif.h"
#include "context.h"
#include "globalcontext.h"
#include "iff.h"
#include "module.h"
#include "utils.h"
#include "term.h"

const void *avm_partition(int *size);

esp_err_t event_handler(void *ctx, system_event_t *event)
{
    return ESP_OK;
}

void app_main()
{
    nvs_flash_init();
    tcpip_adapter_init();
    ESP_ERROR_CHECK(esp_event_loop_init(event_handler, NULL));

    int size;
    const void *flashed_avm = avm_partition(&size);

    uint32_t startup_beam_size;
    const void *startup_beam;

    printf("Booting file mapped at: %p, size: %i\n", flashed_avm, size);

    GlobalContext *glb = globalcontext_new();

    if (!avmpack_is_valid(flashed_avm, size) || !avmpack_find_section_by_flag(flashed_avm, BEAM_START_FLAG, &startup_beam, &startup_beam_size)) {
        fprintf(stderr, "error: invalid AVM Pack\n");
        abort();
    }

    glb->avmpack_data = flashed_avm;
    glb->avmpack_platform_data = NULL;

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    Context *ctx = context_new(glb);

    printf("Execute\n");
    context_execute_loop(ctx, mod, startup_beam, "start", 0);
    printf("Return value: %lx\n", (long) term_to_int32(ctx->x[0]));

    while(1);
}

const void *avm_partition(int *size)
{
    const esp_partition_t *partition = esp_partition_find_first(ESP_PARTITION_TYPE_DATA, ESP_PARTITION_SUBTYPE_ANY, "main.avm");
    if (!partition) {
        printf("AVM partition not found.\n");
        *size = 0;
        return NULL;

    } else {
        printf("Found AVM partition: size: %i, address: 0x%x\n", partition->size, partition->address);
        *size = partition->size;
    }

    const void *mapped_memory;
    spi_flash_mmap_handle_t unmap_handle;
    if (esp_partition_mmap(partition, 0, partition->size, SPI_FLASH_MMAP_DATA, &mapped_memory, &unmap_handle) != ESP_OK) {
        printf("Failed to map BEAM partition\n");
        abort();
        return NULL;
    }

    UNUSED(unmap_handle);

    return mapped_memory;
}
