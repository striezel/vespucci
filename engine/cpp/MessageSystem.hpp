/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010, 2011, 2015  Thoronador

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ***************************************************************************
*/

#ifndef MESSAGESYSTEM_HPP
#define MESSAGESYSTEM_HPP

#include "BasicCallback.hpp"
#include "Helper.hpp"
#include "SingletonPattern.hpp"

#include <memory>
#include <string>

struct TQueueElem; //forward declaration
/* pointer type for elements of the message queue */
typedef std::shared_ptr<TQueueElem> PQueueElem;
/* record that hold an element of the message queue */
struct TQueueElem
{
  std::string txt;
  TShortStrArr options;
  std::string inputCaption, inputText;
  std::shared_ptr<TBasicCallback> cbRec;
  PQueueElem next;
}; //struct


/* ********
   **** TMessageSystem class
   ****
   **** purpose: holds data of currently shown text messages in game and
   ****          provides interface to add new messages
   ****
   *******
*/
class TMessageSystem
{
  private:
    struct
    {
      PQueueElem first;
      PQueueElem last;
    } msg_queue;

    /* pushes a new message at the end of the message queue

       parameters:
           msg_txt   - text of the message
           opts      - array that contains the answer options to that message
           inCaption - caption for the input field
           inText    - preset text of the input field
           the_cbRec - callback record; used after the message window was
                       closed to handle input/ selection

       remarks:
           If opts contains more than zero options, the player can choose from
           the specified number of options in the message window.
           If opts contains no options, but inCaption is not an empty string,
           the player will get a one-line field to enter a text.
    */
    void EnqueueNewMessage(const std::string& msg_txt, const TShortStrArr& opts, const std::string& inCaption, const std::string& inText, std::shared_ptr<TBasicCallback> the_cb);
  public:
    //text messages
    //msg: record
           std::string txt;
           TShortStrArr options;
           int selected_option;
           std::string inputCaption, inputText;
           std::shared_ptr<TBasicCallback> cbRec;
    //     end;//rec

    /* constructor */
    TMessageSystem();

    /* destructor */
    ~TMessageSystem();

    /* adds a new message with the given text (or better: puts it into the
       message queue)

       paramaters:
           msg_text - the text of the message
    */
    void AddMessageSimple(const std::string& msg_txt);

    /* adds a new message with the given text and options (or better: puts it
       into the message queue)

       paramaters:
           msg_text  - the text of the message
           opts      - the options the player can choose from
           the_cbRec - callback record for callback after player has chosen an
                       option
    */
    void AddMessageOptions(const std::string& msg_txt, const TShortStrArr& opts, std::shared_ptr<TBasicCallback> the_cb);

    /* adds a new message with the given text and an input field (or better:
       puts the message into the message queue)

       paramaters:
           msg_text  - the text of the message
           inCaption - caption of the input field
           inDefault - default (preset) text for the input field
           the_cbRec - callback record for callback after player has made his
                       input
    */
    void AddMessageInput(const std::string& msg_txt, const std::string& inCaption, const std::string& inDefault, std::shared_ptr<TBasicCallback> the_cb);

    /* deletes the current message and replaces it with the next message in
       the message queue
    */
    void DequeueMessage();//de-facto dequeue
};//class


//access to universal instance
TMessageSystem& msg()
{
  return Singleton<TMessageSystem>::get();
}

#endif // MESSAGESYSTEM_HPP
