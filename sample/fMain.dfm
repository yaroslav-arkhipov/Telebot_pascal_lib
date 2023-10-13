object Form12: TForm12
  Left = 0
  Top = 0
  Caption = 'Form12'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 433
    Height = 225
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object btnBotCheck: TButton
    Left = 8
    Top = 248
    Width = 233
    Height = 25
    Caption = 'Check the bot is active'
    TabOrder = 1
    OnClick = btnBotCheckClick
  end
  object btnSendMessage: TButton
    Left = 8
    Top = 311
    Width = 233
    Height = 25
    Caption = 'Send message'
    TabOrder = 2
    OnClick = btnSendMessageClick
  end
  object btnGetMessages: TButton
    Left = 8
    Top = 342
    Width = 233
    Height = 25
    Caption = 'Get Messages'
    TabOrder = 3
    OnClick = btnGetMessagesClick
  end
  object edtMsg: TEdit
    Left = 8
    Top = 282
    Width = 233
    Height = 23
    TabOrder = 4
    TextHint = 'Enter your message...'
  end
  object btnSendLocation: TButton
    Left = 247
    Top = 248
    Width = 169
    Height = 25
    Caption = 'Send location'
    TabOrder = 5
    OnClick = btnSendLocationClick
  end
  object btnSendFile: TButton
    Left = 247
    Top = 279
    Width = 169
    Height = 25
    Caption = 'Send file'
    TabOrder = 6
    OnClick = btnSendFileClick
  end
  object btnSendPhoto: TButton
    Left = 247
    Top = 310
    Width = 169
    Height = 25
    Caption = 'Send photo'
    TabOrder = 7
    OnClick = btnSendPhotoClick
  end
  object btnSendVideo: TButton
    Left = 247
    Top = 342
    Width = 169
    Height = 25
    Caption = 'Send video'
    TabOrder = 8
    OnClick = btnSendVideoClick
  end
end
