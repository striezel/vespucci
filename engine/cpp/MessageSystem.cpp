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

#include "MessageSystem.hpp"

#include "DebugWriter.hpp"

TMessageSystem::TMessageSystem()
: txt(""),
  selected_option(0),
  inputCaption(""),
  inputText(""),
  cbRec(nullptr)
{
  //msg part
  options.clear();
  //queue
  msg_queue.first = nullptr;
  msg_queue.last = nullptr;
}//construc

TMessageSystem::~TMessageSystem()
{
  while ((!txt.empty()) or (msg_queue.first!=nullptr))
  {
    DequeueMessage();
  }//while
}//destruc

void TMessageSystem::AddMessageSimple(const std::string& msg_txt)
{
  #ifdef DEBUG_CODE
  WriteDebugLn("Entered TMessageSystem.AddMessageSimple");
  #endif
  if (txt.empty())
  {
    txt = Trim(msg_txt);
    /*msg.*/options.clear();
    inputCaption.clear();
    inputText.clear();
    cbRec = nullptr;
  }
  else
  {
    //enqueue new message
    TShortStrArr null_opts;
    null_opts.clear();
    EnqueueNewMessage(msg_txt, null_opts, "", "", nullptr);
  }//else
  #ifdef DEBUG_CODE
  WriteDebugLn("Leaving TMessageSystem.AddMessageSimple");
  #endif
}//proc

void TMessageSystem::AddMessageOptions(const std::string& msg_txt, const TShortStrArr& opts, std::shared_ptr<TBasicCallback> the_cb)
{
  #ifdef DEBUG_CODE
  WriteDebugLn("Entered TMessageSystem.AddMessageOptions");
  #endif
  if (/*msg.*/txt.empty())
  {
    txt = Trim(msg_txt)+cSpace60;
    options = opts;
    std::vector<std::string>::size_type i;
    for (i = 0; i < opts.size(); ++i)
      options[i] = Trim(opts[i]).substr(0,59);
    inputCaption = "";
    inputText = "";
    selected_option = 0;
    cbRec = the_cb;
  }
  else
  {
    //enqueue new message
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, opts, "", "", the_cb);
  }//else
  #ifdef DEBUG_CODE
  WriteDebugLn("Leaving TMessageSystem.AddMessageOptions");
  #endif
}//proc

void TMessageSystem::AddMessageInput(const std::string& msg_txt, const std::string& inCaption, const std::string& inDefault, std::shared_ptr<TBasicCallback> the_cb)
{
  #ifdef DEBUG_CODE
  WriteDebugLn("Entered TMessageSystem.AddMessageInput");
  #endif
  if (txt.empty())
  {
    txt = Trim(msg_txt)+cSpace60;
    /*msg.*/options.clear();
    //input caption maximum is half the line (i.e. 30 characters)
    inputCaption = Trim(inCaption).substr(0, 30);
    inputText = Trim(inDefault);
    selected_option = 0;
    cbRec = the_cb;
  }//if
  else
  {
    //enqueue new message
    TShortStrArr null_opts;
    null_opts.clear();
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, null_opts, inCaption, inDefault, the_cb);
  }//else
  #ifdef DEBUG_CODE
  WriteDebugLn("Leaving TMessageSystem.AddMessageInput");
  #endif
}//proc

void TMessageSystem::EnqueueNewMessage(const std::string& msg_txt, const TShortStrArr& opts, const std::string& inCaption, const std::string& inText, std::shared_ptr<TBasicCallback> the_cb)
{
  #ifdef DEBUG_CODE
  WriteDebugLn("Entered TMessageSystem.EnqueueNewMessage");
  #endif
  PQueueElem temp = std::make_shared<TQueueElem>();
  temp->txt = msg_txt;
  temp->options = opts;
  std::vector<std::string>::size_type i;
  for (i = 0; i < opts.size(); ++i)
    temp->options[i] = Trim(opts[i]).substr(0, 59);
  //maximum caption is half the line long (i.e. 30 characters)
  temp->inputCaption = Trim(inCaption).substr(0, 30);
  temp->inputText = Trim(inText);
  temp->cbRec = the_cb;
  temp->next = nullptr;
  if (msg_queue.first==nullptr)
  {
    msg_queue.first = temp;
    msg_queue.last = temp;
  }//if
  else
  {
    msg_queue.last->next = temp;
    msg_queue.last = temp;
  }//else
  #ifdef DEBUG_CODE
  WriteDebugLn("Leaving TMessageSystem.EnqueueNewMessage");
  #endif
}//proc

void TMessageSystem::DequeueMessage()
{
  if (msg_queue.first!=nullptr)
  {
    txt = msg_queue.first->txt;
    options = msg_queue.first->options;
    std::vector<std::string>::size_type i;
    for (i = 0; i < msg_queue.first->options.size(); ++i)
      options[i] = msg_queue.first->options[i];
    inputCaption = msg_queue.first->inputCaption;
    inputText = msg_queue.first->inputText;
    cbRec = msg_queue.first->cbRec;
    //move first pointer to new first element
    PQueueElem temp = msg_queue.first;
    msg_queue.first = msg_queue.first->next;
    if (msg_queue.first==nullptr)
      msg_queue.last = nullptr;
    //shorten queue (and thus free former first element)
    temp.reset();
    selected_option = 0;
  }//if-then-branch
  else
  {
    //no new messages in queue; clear msg.
    txt = "";
    options.clear();
    selected_option = 0;
    inputCaption = "";
    inputText = "";
    cbRec = nullptr;
  }//else branch
}//proc
