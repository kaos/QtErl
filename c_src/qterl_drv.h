/**
 * Copyright 2013 Andreas Stenius
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */


#ifndef QTERL_DRV_H
#define QTERL_DRV_H

#if defined(WIN32) && !defined(__WIN32__)
#define __WIN32__
#endif
#include <ei.h>

// opaque state for the outside world
typedef struct qte_state_s *qte_state_t;

// typedef ref type so we can change our minds later without breaking a leg or two
typedef ei_x_buff qte_ref_t;

qte_ref_t *qte_prepare_ref(qte_state_t state, qte_ref_t *in);
qte_ref_t *qte_copy_ref(qte_state_t state, qte_ref_t *dst);
qte_ref_t *qte_copy_ref(qte_ref_t *src, qte_ref_t *dst);
void qte_close_ref(qte_state_t state);
void qte_free_ref(qte_ref_t *ref);

ei_x_buff *qte_prepare_ext(qte_state_t state);
int qte_send_ext(qte_state_t state, ei_x_buff *ext, qte_ref_t *ref = NULL);

#define QTE_OPEN_EREF(_state_, _ref_)       \
  qte_ref_t __ref__;                        \
  ei_x_encode_version(qte_prepare_ref((_state_), &__ref__));  \
  ei_x_encode_ref(&__ref__, (_ref_));

#define QTE_OPEN_REF(_state_, ...)          \
  qte_ref_t __ref__;                        \
  ei_x_format(qte_prepare_ref((_state_), &__ref__), __VA_ARGS__);

// for symetri with QTE_OPEN_REF
#define QTE_CLOSE_REF(_state_) qte_close_ref(_state_)

#define QTE_SEND(_state_, ...) QTE_REF_SEND((_state_), NULL, __VA_ARGS__)
#define QTE_REF_SEND(_state_, _ref_, ...)   \
{                                           \
  ei_x_buff *x = qte_prepare_ext(_state_);  \
  ei_x_format(x, __VA_ARGS__);              \
  qte_send_ext((_state_), x, (_ref_));      \
  ei_x_free(x);                             \
}

#endif // QTERL_DRV_H
