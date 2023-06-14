
function MyButton({onClick,name,color}) {
    return (
        <div
            className="booster"
            style={{backgroundColor: color }}
            onClick={onClick}
        >
            {name}<span class="load loading"></span>
        </div>
    );
}

export default MyButton;
