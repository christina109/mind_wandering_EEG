function cellOutput = computeEEGtopo_group(subs, tasks, states, contentsets, timewin, measure, type)
% Tasks: cell of 'sart', 'vs' or 'merge'
% Contentsets: cell of contentsets(e.g., 1i, 1c)ordered by the specified STATE order; [] - use defaul state definition
% Timewin: the time window to average data
% Measure: e.g., 'p3', 'alpha'
% Type: 'raw' - data from raw matrix; 'marker' - data as features in modelling
% Return is the data for topograph with length of nchan
% Author: Christina Jin (christina.mik109@gmail.com)
% Last edit: Nov 21, 2018

    cd('c:\\topic_mind wandering\\3data\\')
    
    load('pars', 'times')
    
    if ~isempty(contentsets)
        load('pars', 'content_code')
    end
    
    if ismember(lower(measure), {'alpha', 'theta'})
        load('pars_marker.mat', 'bands')
        id = find(strcmp({bands.name}, measure));
        bandrange = bands(id).range;
    end
    
    timeIdx = dsearchn(times', timewin');
    
    for ti = 1:length(tasks)
        
        task = tasks{ti};
        
        for si = 1:length(states)
            
            state = states{si};
            
            % get state definition
            if ~isempty(contentsets)               
                contentset = contentsets{si};
                % disp to check
                if ti == 1
                    disp(['State is ', state, '. Contents are: ', strjoin(contentset)])
                end
                
                % get triggers
                for contenti = 1:length(contentset)
                    content = contentset{contenti};
                    if ~strcmpi(task, 'merge')
                        temp_triggers = [content_code(strcmpi({content_code.task}, task) & strcmpi({content_code.content}, content)).triggers];
                    else 
                        temp_triggers = [content_code(strcmpi({content_code.content}, content)).triggers];
                    end
                    
                    % concatenate
                    if contenti == 1
                        triggers = temp_triggers;
                    else 
                        triggers = [triggers, temp_triggers];
                    end
                end
            end
            
            % loop over subs to get data: nChan x nPnt x nSub
            for subi = 1:length(subs)                    
                sub = subs(subi);
                
                % load data
                if (isempty(contentsets))
                    if strcmp(type, 'raw')
                        temp = get_data(sub, task, state);
                    elseif strcmp(type, 'marker')
                        idmat = csvread(['trialIdx4ml_csvfile\\', num2str(sub),'.csv'], 1, 1);
                        temp = get_data(sub, task, state, idmat);
                    else
                        error('Invalid type!')
                    end
                else 
                    temp = select_trials2(sub, {'content', triggers});
                end
                
                % compute erp or band power
                if ismember(measure, {'p1', 'n1', 'p3'})
                    temp = mean(temp, 3);      
                elseif ismember(measure, {'theta', 'alpha'}) 
                    dataFilt = hilbertFilter(temp, srate, bandrange, 0);
                    power = compute_power(dataFilt); 
                    temp = mean(power, 3);
                else
                    error('Invalid measure!')
                end
                
                % another option: pool trials from all subs, then average across trials?
                % Danger: giving more weights to participant of more data
               
                % intialize
                if subi == 1
                    data = zeros([size(temp,1), size(temp,2), length(subs)]);
                end
                data(:,:,subi) = temp;
            end  % loop over subs
            
            data = squeeze(mean(data,3));
            data2plot = mean(data(:, timeIdx(1):timeIdx(end)),2);
            
            % generate output
            cellOutput((ti-1)*2+si).data = data2plot;
            cellOutput((ti-1)*2+si).task = task;
            cellOutput((ti-1)*2+si).state = state;
            cellOutput((ti-1)*2+si).contentset = contentset;
            cellOutput((ti-1)*2+si).triggers = triggers;
            cellOutput((ti-1)*2+si).subs = subs;
            cellOutput((ti-1)*2+si).timewin = timewin;
            cellOutput((ti-1)*2+si).measure = measure;
            cellOutput((ti-1)*2+si).type = type;
                       
        end  % loop over states
    end  % loop over tasks
    
end  % func