
function GetChannel(name)
  for i = 0, reaper.GetNumAudioInputs() - 1 do
    local channelName = reaper.GetInputChannelName(i)
    if channelName == name then return i
    end
  end
  return -1
end  

function CreateFolder(index, name)
  reaper.InsertTrackAtIndex(index, false)
  folder = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(folder, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( folder, 'I_FOLDERDEPTH',1)
  reaper.SetMediaTrackInfo_Value(folder, 'I_FOLDERCOMPACT', 0)
end

function CreateTrack(index, name, channel, lastInFolder)
  local ch = GetChannel(channel)
  local folderDepth = 0
  if lastInFolder then folderDepth = -1 end
  
  reaper.InsertTrackAtIndex(index, false)
  tr = reaper.GetTrack(0,index)
  reaper.GetSetMediaTrackInfo_String(tr, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( tr, 'I_RECARM',1)
  reaper.SetMediaTrackInfo_Value( tr, 'I_RECINPUT',1024 + ch)
  reaper.SetMediaTrackInfo_Value( tr, 'I_FOLDERDEPTH',folderDepth)
end

CreateFolder(0, "Low noise")
CreateTrack(1, "Low noise 1", "Input 1 (BlackHole 64ch)", false)
CreateTrack(2, "Low noise 2", "Input 3 (BlackHole 64ch)", true)

CreateFolder(3, "Low pitch")
CreateTrack(4, "Low pitch 1", "Input 5 (BlackHole 64ch)", false)
CreateTrack(5, "Low pitch 2", "Input 7 (BlackHole 64ch)", false)
CreateTrack(6, "Low pitch 3", "Input 9 (BlackHole 64ch)", true)

CreateFolder(7, "Middle noise")
CreateTrack(8, "Middle noise 1", "Input 11 (BlackHole 64ch)", false)
CreateTrack(9, "Middle noise 2", "Input 13 (BlackHole 64ch)", true)

CreateFolder(10, "Middle pitch")
CreateTrack(11, "Middle pitch 1", "Input 15 (BlackHole 64ch)", false)
CreateTrack(12, "Middle pitch 2", "Input 17 (BlackHole 64ch)", false)
CreateTrack(13, "Middle pitch 3", "Input 19 (BlackHole 64ch)", true)




