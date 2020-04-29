function [val, x, y] = local_peak(mat, xRng, yRng, xRng2search, yRng2search, sign, plotOn)
% [val, x, y] = local_peak(mat, *xRng = [autoCompute], *yRng = [autoCompute], *xRng2search=[all], *yRng2search=[all], *sign = 1, *plotOn = 1)

% defaults
if nargin < 3 || (isempty(xRng) && isempty(yRng))
    xRng = 1:size(mat,2);
    yRng = 1:size(mat,1);
end

if nargin < 5 || (isempty(xRng2search) && isempty(yRng2search))
    xRng2search = xRng; 
    yRng2search = yRng;    
end

if nargin < 6 || isempty(sign)
    warning('Unspecified sign! Auto look for postive peak (sign = 1).')
    sign = 1;
end

if nargin < 7 
    plotOn = 1;
end

% convert to vertical vector
if isrow(xRng); xRng = xRng'; end
if isrow(yRng); yRng = yRng'; end
if isrow(xRng2search); xRng2search = xRng2search'; end
if isrow(yRng2search); yRng2search = yRng2search'; end

% xid, yid
xIdx2search = dsearchn(xRng,xRng2search);
yIdx2search = dsearchn(yRng,yRng2search);

% search matrix (0 = omit, 1 = search range)
pos2search = zeros(size(mat));
pos2search(yIdx2search(1):yIdx2search(2),xIdx2search(1):xIdx2search(2)) = 1;
% plot to check
% imagesc(xRng, yRng, pos2search)

% local peak area in the specified region
if sign == 1
    pos4target = imregionalmax(mat).* pos2search;
    [r, c] = find(pos4target);
elseif sign == -1
    pos4target = imregionalmin(mat).* pos2search;
    [r, c] = find(pos4target);
else 
    error('Illegal sign (1/-1)!')
end

% decide local peak
if isempty(r) == 0
    vals = diag(mat(r, c));
    if sign == 1
        [val, id] = max(vals);
    elseif sign == -1
        [val, id] = min(vals);
    end
    x = xRng(c(id));
    y = yRng(r(id));
else
    val = 0; x = 0; y = 0;
end

% plot
if plotOn == 1
    
    figure
    imagesc(xRng,yRng,mat)
    set(gca,'YDir','normal')
    % caxis([-200 200])
    hold on 
    x1 = xRng2search(1); y1 = yRng2search(1); x2 = xRng2search(2); y2 = yRng2search(2);
    xvec = [x1, x2, x2, x1, x1];
    yvec = [y1, y1, y2, y2, y1];
    plot(xvec, yvec, 'k:', 'Color',[0.25 0.25 0.25], 'LineWidth', 2);
    if isempty(r) == 0
        hold on
        plot(x, y, '--ks','MarkerSize',5,'MarkerFaceColor','k');
        hold on
        text(x+10, y,num2str(val));
    end
    colorbar
    
end

end % func