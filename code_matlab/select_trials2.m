function [data, idx, rt] = select_trials2(sub, argcell)
% extract trials based on the given trigger sets, use [] to select all.
% This is an extended version of select_trials() in that it allows trigger 
% combo based not only on mental state (eventnum) but also on orientation, 
% stickiness or emotion. 
% data = select_trials2(sub, {'state'/'content', s_triggers, 'orientation', o_triggers, 
%        'stickiness', s_triggers, 'emotion', e_triggers})
% data: nChan x nPnt x nTrial
% last edited: July 25, 2018

if nargin == 1 || isempty(argcell)
    disp('All trials are selected.')
    content = [];
    emotion = [];
    stickiness = [];
    orientation = [];
else
    cid = find(strcmp(argcell, 'state'));
    if isempty(cid)
         cid = find(strcmp(argcell, 'content'));
    end
    eid = find(strcmp(argcell, 'emotion'));
    sid = find(strcmp(argcell, 'stickiness'));
    oid = find(strcmp(argcell, 'orientation'));
    
    if ~isempty(cid)
        content = argcell{cid + 1};
        disp(['Select data base on content: ', num2str(content)])
    else
        content = [];
        disp(['Select data base on content: all'])
    end

    if ~isempty(eid)
        emotion = argcell{eid + 1};
        disp(['Select data base on emotion: ', num2str(emotion)])
    else 
        emotion = [];
        disp(['Select data base on emotion: all'])
    end

    if ~isempty(sid)
        stickiness = argcell{sid + 1};
        disp(['Select data base on stickiness: ', num2str(stickiness)])
    else
        stickiness = [];
        disp(['Select data base on stickiness: all'])
    end

    if ~isempty(oid)
        orientation = argcell{oid + 1};
        disp(['Select data base on orientation: ', num2str(orientation)])
    else 
        orientation = [];
        disp(['Select data base on orientation: all'])
    end
end

for i = 1:2 %over session
    
    f = [num2str(sub), '_', num2str(i), '_epochs_ica_a.set'];
    if exist(f, 'file') ~= 2
        f = [num2str(sub), '_', num2str(i), '_epochs_ica_o_a.set'];
    end
    
    EEG = pop_loadset(f);
    
    trials = ~EEG.reject.rejmanual;  
    
    if ~isempty(content)
        trials = ismember([EEG.event.new], content) & trials;   % logical array with length as trial count
    end
    if ~isempty(orientation)
        trials = ismember([EEG.event.o], orientation) & trials; 
    end
    if ~isempty(stickiness)
        trials = ismember([EEG.event.s], stickiness) & trials; 
    end
    if ~isempty(emotion)
        trials = ismember([EEG.event.e], emotion) & trials; 
    end
    
    temp  = EEG.data(:,:,trials);
    temp2 = [EEG.event.urevent]; temp2 = temp2(trials);
    temp3 = [EEG.event.rt];      temp3 = temp3(trials);
       
    if i==1
        data = temp; 
        idx  = [temp2' zeros(length(temp2),1)];  % add a same length column with 0 
        rt   = temp3;
    else
        data(:,:,end+1:end+size(temp,3)) = temp;
        idx = [idx; [zeros(length(temp2),1) temp2']];
        rt  = [rt temp3];
    end
    
end %i: session

end %func