/* GStreamer GstTagSetter interface unit tests
 * Copyright (C) 2007 Tim-Philipp Müller <tim centricular net>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <gst/check/gstcheck.h>
#include <gst/gst.h>
#include <string.h>

/* some minimal GstTagSetter object */
#define GST_TYPE_DUMMY_ENC gst_dummy_enc_get_type()

typedef GstElement GstDummyEnc;
typedef GstElementClass GstDummyEncClass;

static void gst_dummy_enc_add_interfaces (GType enc_type);

GType gst_dummy_enc_get_type (void);
GST_BOILERPLATE_FULL (GstDummyEnc, gst_dummy_enc, GstElement,
    GST_TYPE_ELEMENT, gst_dummy_enc_add_interfaces);

static void
gst_dummy_enc_add_interfaces (GType enc_type)
{
  static const GInterfaceInfo tag_setter_info = { NULL, NULL, NULL };

  g_type_add_interface_static (enc_type, GST_TYPE_TAG_SETTER, &tag_setter_info);
}

static void
gst_dummy_enc_base_init (gpointer g_class)
{
}

static void
gst_dummy_enc_class_init (GstDummyEncClass * klass)
{
}

static void
gst_dummy_enc_init (GstDummyEnc * enc, GstDummyEncClass * klass)
{
}

static void
tag_list_foreach (const GstTagList * taglist, const gchar * tag, guint * p_num)
{
  guint tag_size;

  tag_size = gst_tag_list_get_tag_size (taglist, tag);
  GST_LOG ("%u+%u tag = %s", *p_num, tag_size, tag);
  *p_num += tag_size;
}

static guint
tag_setter_list_length (GstTagSetter * setter)
{
  guint len = 0;

  if (gst_tag_setter_get_tag_list (setter) == NULL)
    return 0;

  gst_tag_list_foreach (gst_tag_setter_get_tag_list (setter),
      (GstTagForeachFunc) tag_list_foreach, &len);
  return len;
}

static guint
tag_list_length (const GstTagList * tag_list)
{
  guint len = 0;

  if (tag_list == NULL)
    return 0;

  gst_tag_list_foreach (tag_list, (GstTagForeachFunc) tag_list_foreach, &len);
  return len;
}

#define assert_tag_setter_list_length(setter,len) \
    fail_unless_equals_int (tag_setter_list_length(setter), len);

GST_START_TEST (test_merge)
{
  GstTagSetter *setter;
  GstTagList *list1, *list2;
  GstElement *enc;

  enc = g_object_new (GST_TYPE_DUMMY_ENC, NULL);
  fail_unless (enc != NULL);

  setter = GST_TAG_SETTER (enc);

  list1 = gst_tag_list_new ();
  gst_tag_list_add (list1, GST_TAG_MERGE_APPEND, GST_TAG_ARTIST, "artist1",
      NULL);
  gst_tag_setter_merge_tags (setter, list1, GST_TAG_MERGE_APPEND);
  assert_tag_setter_list_length (setter, 1);

  list2 = gst_tag_list_new ();
  gst_tag_list_add (list2, GST_TAG_MERGE_APPEND, GST_TAG_ARTIST, "artist2",
      GST_TAG_TITLE, "title1", NULL);
  gst_tag_setter_merge_tags (setter, list2, GST_TAG_MERGE_APPEND);
  assert_tag_setter_list_length (setter, 3);

  gst_tag_setter_merge_tags (setter, list2, GST_TAG_MERGE_REPLACE_ALL);
  assert_tag_setter_list_length (setter, 2);

  gst_tag_setter_merge_tags (setter, list1, GST_TAG_MERGE_REPLACE_ALL);
  assert_tag_setter_list_length (setter, 1);

  gst_tag_setter_add_tags (setter, GST_TAG_MERGE_APPEND, GST_TAG_ALBUM, "xyz",
      NULL);
  assert_tag_setter_list_length (setter, 2);

  gst_tag_list_free (list2);
  gst_tag_list_free (list1);

  g_object_unref (enc);
}

GST_END_TEST
GST_START_TEST (test_merge_modes)
{
  GstTagMergeMode mode;

  for (mode = GST_TAG_MERGE_REPLACE_ALL; mode < GST_TAG_MERGE_COUNT; mode++) {
    gint i;

    for (i = 0; i < 4; i++) {
      GstElement *enc;
      GstTagSetter *setter;
      GstTagList *list1, *list2, *merged;

      enc = g_object_new (GST_TYPE_DUMMY_ENC, NULL);
      fail_unless (enc != NULL);

      setter = GST_TAG_SETTER (enc);
      list1 = gst_tag_list_new ();
      list2 = gst_tag_list_new ();

      /* i = 0: -     -
       * i = 1: list1 -
       * i = 2: -     list2
       * i = 3: list1 list2 */

      if (i % 2 == 1) {
        gst_tag_list_add (list1, GST_TAG_MERGE_APPEND, GST_TAG_ARTIST,
            "artist1", NULL);
      }
      if (i > 1) {
        gst_tag_list_add (list2, GST_TAG_MERGE_APPEND, GST_TAG_ARTIST,
            "artist2", NULL);
      }

      gst_tag_setter_merge_tags (setter, list1, GST_TAG_MERGE_APPEND);
      gst_tag_setter_merge_tags (setter, list2, mode);

      merged = gst_tag_list_merge (list1, list2, mode);

      fail_unless_equals_int (tag_list_length (gst_tag_setter_get_tag_list
              (setter)), tag_list_length (merged));

      gst_tag_list_free (list1);
      gst_tag_list_free (list2);
      gst_tag_list_free (merged);
      gst_object_unref (enc);
    }
  }
}

GST_END_TEST
GST_START_TEST (test_merge_modes_skip_empty)
{
  GstTagMergeMode mode;

  for (mode = GST_TAG_MERGE_REPLACE_ALL; mode < GST_TAG_MERGE_COUNT; mode++) {
    gint i;

    for (i = 0; i < 2; i++) {
      GstElement *enc;
      GstTagSetter *setter;
      GstTagList *list1, *list2, *merged;

      enc = g_object_new (GST_TYPE_DUMMY_ENC, NULL);
      fail_unless (enc != NULL);

      setter = GST_TAG_SETTER (enc);
      list1 = gst_tag_list_new ();
      list2 = gst_tag_list_new ();

      if (i == 1) {
        gst_tag_list_add (list2, GST_TAG_MERGE_APPEND, GST_TAG_ARTIST,
            "artist2", NULL);
      }

      gst_tag_setter_merge_tags (setter, list2, mode);

      merged = gst_tag_list_merge (list1, list2, mode);

      fail_unless_equals_int (tag_list_length (gst_tag_setter_get_tag_list
              (setter)), tag_list_length (merged));

      gst_tag_list_free (list1);
      gst_tag_list_free (list2);
      gst_tag_list_free (merged);
      gst_object_unref (enc);
    }
  }
}

GST_END_TEST static int spin_and_wait = 1;
static int threads_running = 0;

#define THREADS_TEST_SECONDS 1.5

static gpointer
test_threads_thread_func1 (gpointer data)
{
  GstTagSetter *setter = GST_TAG_SETTER (data);
  GTimer *timer;

  timer = g_timer_new ();

  g_atomic_int_inc (&threads_running);
  while (g_atomic_int_get (&spin_and_wait))
    g_usleep (0);

  GST_INFO ("Go!");
  g_timer_start (timer);

  while (g_timer_elapsed (timer, NULL) < THREADS_TEST_SECONDS) {
    gst_tag_setter_add_tags (setter, GST_TAG_MERGE_APPEND, GST_TAG_ARTIST,
        "some artist", GST_TAG_TITLE, "some title", GST_TAG_TRACK_NUMBER, 6,
        NULL);
  }

  g_timer_destroy (timer);
  GST_INFO ("Done");

  return NULL;
}

static gpointer
test_threads_thread_func2 (gpointer data)
{
  GstTagSetter *setter = GST_TAG_SETTER (data);
  GTimer *timer;

  timer = g_timer_new ();

  g_atomic_int_inc (&threads_running);
  while (g_atomic_int_get (&spin_and_wait))
    g_usleep (0);

  GST_INFO ("Go!");
  g_timer_start (timer);

  while (g_timer_elapsed (timer, NULL) < THREADS_TEST_SECONDS) {
    gst_tag_setter_add_tags (setter, GST_TAG_MERGE_PREPEND, GST_TAG_CODEC,
        "MP42", GST_TAG_COMMENT, "deep insights go here", GST_TAG_TRACK_COUNT,
        10, NULL);
  }

  g_timer_destroy (timer);
  GST_INFO ("Done");

  return NULL;
}

static gpointer
test_threads_thread_func3 (gpointer data)
{
  GstTagSetter *setter = GST_TAG_SETTER (data);
  GTimer *timer;

  timer = g_timer_new ();

  g_atomic_int_inc (&threads_running);
  while (g_atomic_int_get (&spin_and_wait))
    g_usleep (0);

  GST_INFO ("Go!");
  g_timer_start (timer);

  while (g_timer_elapsed (timer, NULL) < THREADS_TEST_SECONDS) {
    gst_tag_setter_reset_tags (setter);
  }

  g_timer_destroy (timer);
  GST_INFO ("Done");

  return NULL;
}

GST_START_TEST (test_threads)
{
  GstTagSetter *setter;
  GThread *threads[3];

  setter = GST_TAG_SETTER (g_object_new (GST_TYPE_DUMMY_ENC, NULL));

  spin_and_wait = TRUE;
  threads[0] = g_thread_create (test_threads_thread_func1, setter, TRUE, NULL);
  threads[1] = g_thread_create (test_threads_thread_func2, setter, TRUE, NULL);
  threads[2] = g_thread_create (test_threads_thread_func3, setter, TRUE, NULL);

  while (g_atomic_int_get (&threads_running) < 3)
    g_usleep (10);

  g_atomic_int_set (&spin_and_wait, FALSE);

  g_thread_join (threads[0]);
  g_thread_join (threads[1]);
  g_thread_join (threads[2]);

  g_object_unref (G_OBJECT (setter));
}

GST_END_TEST static Suite *
gst_tag_setter_suite (void)
{
  Suite *s = suite_create ("GstTagSetter");
  TCase *tc_chain = tcase_create ("general");

  suite_add_tcase (s, tc_chain);
  tcase_add_test (tc_chain, test_merge);
  tcase_add_test (tc_chain, test_merge_modes);
  tcase_add_test (tc_chain, test_merge_modes_skip_empty);
  tcase_add_test (tc_chain, test_threads);

  return s;
}

GST_CHECK_MAIN (gst_tag_setter);
