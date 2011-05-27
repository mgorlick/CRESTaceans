#include <gst/gst.h>
#include <gst/gstmessage.h>
#include <glib.h>
#include <stddef.h>


void on_pad_added (GstElement *element, GstPad *pad, gpointer data) {
  GstPad *sinkpad;
  GstElement *decoder = (GstElement *) data;
  g_print ("Dynamic pad created, linking demuxer/decoder\n");
  sinkpad = gst_element_get_static_pad (decoder, "sink");
  gst_pad_link (pad, sinkpad);
  gst_object_unref (sinkpad);
}


typedef void (*VoidMessageType) (GstMessage *message);

void signal_connect_message (gpointer instance, gchar* detailed_signal, VoidMessageType handler, gpointer data) {
  g_signal_connect (instance, detailed_signal, G_CALLBACK (handler), data);
}


void signal_connect_pad_added (GstElement* e1, GstElement* e2) {
  g_signal_connect (e1, "pad-added", G_CALLBACK (on_pad_added), e2);
}

int gst_message_type (GstMessage* m) {
  return GST_MESSAGE_CAST(m)->type;
}

void gst_message_unref_w (GstMessage* m) {
  gst_message_unref(m);
}

void print_gst_time_format (gint64 pos, gint64 len) {
    g_print ("Time: %" GST_TIME_FORMAT " / %" GST_TIME_FORMAT "\r",
	     GST_TIME_ARGS (pos), GST_TIME_ARGS (len));
}


int pointer_is_null (void* ptr){ 
	if (ptr == NULL) { return 1; } else { return 0; }
}

/* GETTERS AND SETTERS */

int get_GstBin_numchildren (GstBin* b){
	return b->numchildren;
}

gchar* get_GstElement_name (GstElement* elem){
	return gst_element_get_name(elem);
}

gchar* get_GstPad_name (GstPad* pad){
	return gst_pad_get_name(pad);
}
	
void set_Gst_Buffer_Size (GstBuffer* buffer, int bytes){
	GST_BUFFER_SIZE (buffer) = bytes;
}

int get_Gst_Buffer_Size (GstBuffer* buffer){
	return GST_BUFFER_SIZE (buffer);
}
	
void set_Gst_Buffer_Data (GstBuffer* buffer, guint8* data)
{
	GST_BUFFER_DATA(buffer) = data;
}

guint8 get_Gst_Buffer_Data (GstBuffer* buffer)
{
	return GST_BUFFER_DATA(buffer);
}

void set_Gst_Buffer_Mallocdata (GstBuffer* buffer, guint8* data)
{
	GST_BUFFER_MALLOCDATA(buffer) = data;
}

guint8 get_Gst_Buffer_Mallocdata (GstBuffer* buffer)
{
	return GST_BUFFER_MALLOCDATA(buffer);
}

void set_Gst_Buffer_Offset (GstBuffer* buffer, guint offset)
{
	GST_BUFFER_OFFSET(buffer) = offset;
}

guint8 get_Gst_Buffer_Offset (GstBuffer* buffer)
{
	return GST_BUFFER_OFFSET(buffer);
}

void set_Gst_Buffer_Offset_End (GstBuffer* buffer, guint offset){
	GST_BUFFER_OFFSET_END(buffer) = offset;
}

guint8 get_Gst_Buffer_Offset_End (GstBuffer* buffer){
	return GST_BUFFER_OFFSET_END(buffer);
}

void set_Gst_Buffer_Timestamp (GstBuffer* buffer, GstClockTime nanoseconds){
	GST_BUFFER_TIMESTAMP(buffer) = nanoseconds;
}

GstClockTime get_Gst_Buffer_Timestamp (GstBuffer* buffer){
	return GST_BUFFER_TIMESTAMP(buffer);
}

void set_Gst_Buffer_Caps (GstBuffer* buffer, GstCaps* caps){
	GST_BUFFER_CAPS(buffer) = caps;
}

GstCaps* get_Gst_Buffer_Caps (GstBuffer* buffer){
	return GST_BUFFER_CAPS(buffer);
}

gchar* get_Gst_Pad_Template_Name_Template (GstPadTemplate* templ){
	return GST_PAD_TEMPLATE_NAME_TEMPLATE(templ);
}

gchar* get_gst_element_get_name (GstElement* elem){
	return gst_element_get_name(elem);
}

void set_gst_element_set_name (GstElement* elem, gchar* name){
	gst_element_set_name(elem,name);
}

int get_Gst_State (GstElement* elem){
	return GST_STATE(elem);
}

int get_Gst_State_Next (GstElement* elem){
	return GST_STATE_NEXT(elem);
}

int get_Gst_State_Return (GstElement* elem){
	return GST_STATE_RETURN(elem);
}

gchar* get_gst_pad_get_name (GstPad* pad){
	return gst_pad_get_name(pad);
}	


int get_Gst_Version_Major (){
	return GST_VERSION_MAJOR;
}

int get_Gst_Version_Minor (){
 	return GST_VERSION_MINOR;
}

int get_Gst_Version_Micro (){
	return GST_VERSION_MICRO;
}

int get_Gst_Version_Nano (){
	return GST_VERSION_NANO;
}

guint32 get_GST_CORE_ERROR(){
	return GST_CORE_ERROR;
}

guint32 get_GST_LIBRARY_ERROR(){
	return GST_LIBRARY_ERROR;
}

guint32 get_GST_RESOURCE_ERROR(){
	return GST_RESOURCE_ERROR;
}

guint32 get_GST_STREAM_ERROR(){
	return GST_STREAM_ERROR;
}

guint32 get_GST_ERROR_SYSTEM(){
	return GST_ERROR_SYSTEM;
}

uint get_gst_caps_refcount(GstCaps* caps){
	return GST_CAPS_REFCOUNT(caps);
}

GType get_gst_type_fourcc(){
 return GST_TYPE_FOURCC;
}

guint32 get_gst_make_fourcc(char a, char b, char c, char d){
	return GST_MAKE_FOURCC(a,b,c,d);
}

GstStaticCaps* get_gst_static_caps (gchar* string){
	GstStaticCaps the_caps = GST_STATIC_CAPS (string);
	GstStaticCaps *caps = malloc(sizeof (GstStaticCaps));
	*caps = the_caps;
	return caps;
}
