function edit_triggers(sub,session)
% edit_triggers(sub,session)
% to modify the EEG.event and EEG.urevent through adding information from
% an external .csv file containing trigger information (in folder: \\preprocessing\\ev_ed)
% last edited: Jun 5, 2018

cd('c:\\topic_mind wandering\\3data')

f = ['preprocessing\\',num2str(sub),'_',num2str(session),'_epochs_ica_a.set'];

if exist(f, 'file') ~= 2
    f = ['preprocessing\\',num2str(sub),'_',num2str(session),'_epochs_ica_o_a.set'];
end 
EEG = pop_loadset(f);

% find the startId of task triggers & check 
startId  = find([EEG.urevent.type]<30 & [EEG.urevent.type]>9, 1);
[~, obs] = size(EEG.urevent);
if startId-1+825 ~= obs
    warning(['Check the events of Sub' num2str(sub) ' in Session ' num2str(session) '!'])
end

beh = csvread(['preprocessing\\ev_ed\\ev_ed',num2str(sub),'_s',num2str(session),'.csv'],1,1);

if size(beh,1) ~= 825
    error('Incorrect imported trigger count!')
end
if size(EEG.urevent,2) ~= startId+824
    error('Incorrect first task trigger id!')
end

beh  = [zeros(startId-1,6); beh];  % for EEG.urevent, beh(:,i) 1-eventnum, 2-rt, 3-correct, 4-orientation, 5-stickiness, 6-emotion
beh2 = beh([EEG.event.urevent],:); % for EEG.event

ev     = num2cell(beh(:,1));  ev2     = num2cell(beh2(:,1));
rt     = num2cell(beh(:,2));  rt2     = num2cell(beh2(:,2));
orient = num2cell(beh(:,4));  orient2 = num2cell(beh2(:,4));
stick  = num2cell(beh(:,5));  stick2  = num2cell(beh2(:,5));
emo    = num2cell(beh(:,6));  emo2    = num2cell(beh2(:,6));

[EEG.urevent.new] = ev{:};     [EEG.event.new] = ev2{:};
[EEG.urevent.rt]  = rt{:};     [EEG.event.rt]  = rt2{:};
[EEG.urevent.o]   = orient{:}; [EEG.event.o]   = orient2{:};
[EEG.urevent.s]   = stick{:};  [EEG.event.s]   = stick2{:};
[EEG.urevent.e]   = emo{:};    [EEG.event.e]   = emo2{:};

pop_saveset(EEG, 'filename', f); % overwrite the old file because only new info is added to it and this saves the storage

end %func
