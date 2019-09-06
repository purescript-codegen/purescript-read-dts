export interface BadgeOrigin {
  vertical: 'top' | 'bottom';
  horizontal: 'left' | 'right';
}

export interface BadgeProps
// extends StandardProps<React.HTMLAttributes<HTMLDivElement>, BadgeClassKey> 
{
  anchorOrigin?: BadgeOrigin;
  overlap?: 'rectangle' | 'circle';
  // badgeContent?: React.ReactNode;
  // children: React.ReactNode;
  color?: 'primary' | 'secondary' | 'default' | 'error';
  // component?: React.ElementType<React.HTMLAttributes<HTMLDivElement>>;
  invisible?: boolean;
  max?: number;
  showZero?: boolean;
  textval: text;
  variant?: 'standard' | 'dot';
  tree: Tree<BadgeProps>;
}

export type text = string;

export type Tree<T> = { value: T } | { children: Tree<T> };

export type AliasOrInterface = text | BadgeProps;

export type BadgeClassKey =
  | 'root'
  | 'badge'
  | 'colorPrimary'
  | 'colorSecondary'
  | 'colorError'
  | 'dot'
  | 'anchorOriginTopRightRectangle'
  | 'anchorOriginBottomRightRectangle'
  | 'anchorOriginTopLeftRectangle'
  | 'anchorOriginBottomLeftRectangle'
  | 'anchorOriginTopRightCircle'
  | 'anchorOriginBottomRightCircle'
  | 'anchorOriginTopLeftCircle'
  | 'invisible';

// export default function Badge(props: BadgeProps): JSX.Element | null;
