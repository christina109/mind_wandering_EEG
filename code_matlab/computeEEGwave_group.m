function cellOutput = computeEEGwave_group(subs, tasks, states, contentsets, chans, measure, type, f_marker, averageOn)
% Tasks: cell of 'sart', 'vs' or 'merge'
% Contentsets: cell of contentsets(e.g., 1i, 1c)ordered by the specified STATE order; [] - use defaul state definition
% Chans: channels used to average data
% Measure: e.g., 'p3', 'alpha'
% Type: 'raw' - data from raw matrix; 'marker' - data as features in modelling
% F_marker: folders containing measurements of markers 
% AverageOn: 1(default) - to return group mean and se-within, other values - to return individual mean
% Return is the data for wavegraph with length of npnt
% Author: Christina Jin (christina.mik109@gmail.com)
% Last edit: Nov 21, 2018

    cd('c:\\topic_mind wandering\\3data\\')
    
    load('pars', 'times')
    
    if ~isempty(contentsets)
        load('pars', 'content_code')
    end
    
    if isempty(averageOn)
        averageOn = 1;
    end
    
    if ismember(lower(measure), {'alpha', 'theta'})
        load('pars_marker.mat', 'bands')
        id = find(strcmp({bands.name}, measure));
        bandrange = bands(id).range;
    end
    
        
    for ti = 1:length(tasks)

        task = tasks{ti};

        for si = 1:length(states)

            state = states{si};
            
            % get raw data
            if strcmpi(type, 'raw')
                % get state definition
                if ~isempty(contentsets)               
                    contentset = contentsets{si};
                    % disp to check
                    disp(['State is ', state, '. Contents are: ', strjoin(contentset)])

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
                
                % loop over subs to get data 
                for subi = 1:length(subs)                    
                    sub = subs(subi);

                    % load data
                    % load by the default condition
                    if (isempty(contentsets))
                        temp = get_data(sub, task, state);
                    % load by the specified contentsets
                    else
                        temp = select_trials2(sub, {'content', triggers});
                    end
                    
                    % compute mean
                    if ismember(measure, {'p1', 'n1', 'p3'})
                        temp = mean(temp,3); 
                    elseif ismember(measure, {'theta', 'alpha'}) 
                        dataFilt = hilbertFilter(temp, srate, bandrange, 0);
                        power = compute_power(dataFilt); 
                        temp = mean(power, 3);
                    else
                        error('Invalid measure!')
                    end
                    
                    % register
                    if subi == 1
                        data = zeros([size(temp,1), size(temp,2), length(subs)]);
                    end
                    data(:,:,subi) = temp;
                    
                end  % loop over subs
                
            % get recovered data based on marker
            elseif strcmpi(type, 'marker')
                % state definition
                if ~isempty(contentsets)               
                    contentset = contentsets{si};
                    % disp to check
                    disp(['State is ', state, '. Contents are: ', strjoin(contentset)])
                end
                
                for subi = 1:length(subs)                    
                    sub = subs(subi);
  
                    if ~ismember(measure, {'p1', 'n1', 'p3'})
                        error('type = marker can only be set when the specified measure is an ERP!')
                    end
                    
                    % load measures and recover the signal
                    % load based on default contentsets
                    if isempty(contentsets)
                         for chani = 1:length(chans)
                             chan = chans(chani);
                             load([f_marker, '\\', lower(measure), ' chan', num2str(chan), '\\', num2str(sub), '.mat'], [measure, '_', task, '_', state]);
                             eval(['Wmat = ', measure, '_', task, '_', state, ';'])
                             %Wmat = Wmat(ismember(Wmat(:,1:2), idmat, 'rows'),3:5);
                             Wmat = Wmat(Wmat(:,3) > 0, 3:5);  % filter out zeros 
                             temp2 = zeros(size(Wmat,1), length(times));
                             % recover the signal
                             for casei = 1:size(Wmat,1)
                                temp2(casei,:) = mexican_hat(times, Wmat(casei,2), Wmat(casei,3)) * Wmat(casei,1);
                             end
                             if chani == 1
                                 temp = temp2;
                             else 
                                 temp = [temp; temp2];
                             end                                    
                         end  % loop over chans
                         
                    % load measures based on the specified condsets
                    else
                         for chani = 1:length(chans)
                             chan = chans(chani);
                             for ci = 1:length(contentset)
                                 content = contentset{ci};
                                 if ~strcmpi(task,'merge')
                                     load([f_marker, '\\', lower(measure), ' chan', num2str(chan), '\\', num2str(sub), '.mat'], [measure, '_', task, '_', content]);
                                     eval(['Wmat = ', measure, '_', task, '_', content, ';'])
                                 else
                                     load([f_marker, '\\', lower(measure), ' chan', num2str(chan), '\\', num2str(sub), '.mat'], [measure, '_sart_', content]);
                                     eval(['Wmat = ', measure, '_sart_', content, ';'])
                                     load([f_marker, '\\', lower(measure), ' chan', num2str(chan), '\\', num2str(sub), '.mat'], [measure, '_vs_', content]);
                                     eval(['Wmat = [Wmat; ', measure, '_vs_', content, '];'])
                                 end
                                 %Wmat = Wmat(ismember(Wmat(:,1:2), idmat, 'rows'),3:5);
                                 Wmat = Wmat(Wmat(:,3) ~= 0, 3:5);  % filter out zeros 
                                 temp2 = zeros(size(Wmat,1), length(times));
                                 % recover the signal
                                 for casei = 1:size(Wmat,1)
                                    temp2(casei,:) = mexican_hat(times, Wmat(casei,2), Wmat(casei,3)) * Wmat(casei,1);
                                 end
                                 % register
                                 if chani == 1 && ci == 1
                                     temp = temp2;
                                 else 
                                     temp = [temp; temp2];
                                 end
                             end  % loop over contents
                         end  % loop over chans                       
                    end
                    
                    % compute idv mean 
                    temp = mean(temp, 1);
                    
                    % register
                    if subi == 1
                        data = zeros(size(temp,2), length(subs));
                    end
                    data(:,subi) = temp;
                                    
                end  % loop over subs
            else
                error('Invalid measure!')
            end
            
            if strcmpi(type, 'raw')
                data = squeeze(mean(data(chans,:,:),1));
            end
            
            if averageOn
                data2plot = mean(data,2);
                % save for computing within-se
                if si == 1 
                    data_raw = zeros([length(times) length(subs) length(states)]);
                end
                data_raw(:,:,si) = data;
                cellOutput((ti-1)*2+si).data = data2plot;
            else
                cellOutput((ti-1)*2+si).data = data;
            end
            
            % generate output
           
            cellOutput((ti-1)*2+si).task = task;
            cellOutput((ti-1)*2+si).state = state;
            cellOutput((ti-1)*2+si).contentset = contentset;
            cellOutput((ti-1)*2+si).subs = subs;
            cellOutput((ti-1)*2+si).chans = chans;
            cellOutput((ti-1)*2+si).measure = measure;
            cellOutput((ti-1)*2+si).type = type;
                      
        end  % loop over states
        
        if averageOn
            % prapare for computing winthin-se
            if ti == 1
                mean_all = zeros(length(times), length(subs), length(tasks) * length(states));
                mean_idv = mean_all;
                data_corr = mean_idv;
            end
            mean_all(:,:,(ti-1)*2+[1:2]) = repmat(mean(mean(data_raw, 3),2), [1 length(subs) length(states)]);
            mean_idv(:,:,(ti-1)*2+[1:2]) = repmat(mean(data_raw, 3), [1, 1, length(states)]);
            data_corr(:,:,(ti-1)*2+[1:2]) = data_raw;
        end
                                                         
    end  % loop over tasks
    
    if averageOn 
        % compute for se
        data_corr = data_corr + mean_all - mean_idv;
        for ti = 1:length(tasks)
            for si = 1:length(states)
                cellOutput((ti-1)*2+si).sewithin = std(data_corr(:, :, (ti-1)*2+si), [], 2)./ sqrt(length(subs)-1);
            end
        end
    end

end  % func
